{-# LANGUAGE NoImplicitPrelude #-}

module Exec where

import Prelude hiding (readFile, mod, snd)
import qualified Prelude
import Parsing (parseFull)
import CPU (printCommands)
import Lang
import Registry
import Data.Text hiding (replicate, zip)
import Control.Monad.Reader
import Control.Concurrent.Async
import Side
import Data.List.Zipper
import Run

binaryInstruction :: (MonadReg m) => (Int -> Int -> Int) -> Register -> Value -> ReaderT ID m ()
binaryInstruction combine reg val = do
    i <- getValue val
    j <- getValue (Right reg)
    setValue reg (Left $ combine j i)

regExec :: (MonadReg m) => RegInstruction -> ReaderT ID m ()
regExec (SET reg val) = setValue reg val
regExec (ADD reg val) = binaryInstruction (+) reg val
regExec (MUL reg val) = binaryInstruction (*) reg val
regExec (MOD reg val) = binaryInstruction (Prelude.mod) reg val

regProgram :: (MonadReg m) => [RegInstruction] -> ReaderT ID m ()
regProgram = sequence_ . fmap regExec

channelExec :: (MonadRun m) => ChannelInstruction -> ReaderT ID m ()
channelExec (SND val) = do
    i <- ask
    if i == 0 then send 1 val else
        if i == 1 then send 0 val
            else send i val
channelExec (RCV val) = callRcv val

controlExec :: (MonadReg m) => (ControlInstruction) -> Zipper Instruction -> ReaderT ID m (Maybe (Zipper Instruction))
controlExec (JGZ xval yval) zipper = do 
    x <- getValue xval
    y <- getValue yval
    if (x <= 0 || y == 0) then pure (Just $ right zipper) else do
        pure (Just $ (zipperFor y zipper))
    where
        leftFor _ (Zip [] _) = Zip [] []
        leftFor 0 zipp = zipp
        leftFor 1 zipp = left zipp
        leftFor n zipp = left $ leftFor (n - 1) zipp

        rightFor 0 zipp = zipp
        rightFor 1 zipp = right zipp
        rightFor n zipp = right $ rightFor (n - 1) zipp

        zipperFor n zipp = if n < 0 then leftFor (-n) zipp else rightFor n zipp

printing :: (MonadRun m) => Maybe Instruction -> Bool -> ReaderT ID m ()
printing (Just inst) weDo = if weDo then printState >> (printInstruction inst) else pure ()
printing Nothing weDo = if weDo then printState else pure ()


step :: (MonadRun m) => Bool -> Zipper Instruction -> ReaderT ID m (Maybe (Zipper Instruction))
step _ (Zip [] []) = pure Nothing
step _ (Zip _ []) = pure Nothing
step printSteps (Zip ls (ChannelInst ci : rs)) = do
    printing (Just . ChannelInst $ ci) printSteps
    channelExec ci
    pure (Just $ Zip (ChannelInst ci : ls) rs)
step printSteps (Zip ls (RegInst p : rs)) = do
    printing (Just . RegInst $ p) printSteps
    regExec p
    pure (Just $ Zip (RegInst p : ls) rs)
step printSteps zipper@(Zip _ (ControlInst ci : _)) = do 
    printing (Just . ControlInst $ ci) printSteps
    zipper' <- controlExec ci zipper
    pure zipper'

program :: (MonadRun m) => Bool -> [Instruction] -> ReaderT ID m ()
program printSteps is = programZipped (fromList is)
    where
        programZipped zipper = do
            cont <- step printSteps zipper
            case cont of
                Nothing -> pure ()
                (Just zipper') -> programZipped zipper'

program' :: (MonadRun m) => Bool -> [Instruction] -> m ()
program' printSteps is = runReaderT (program printSteps is) 0

programAll :: (MonadRun m) => ([m ()] -> a) -> Int -> Bool -> [Instruction] -> a
programAll runIt maxThreads printSteps is = runIt (programs instructions)
    where
        instructions = zip (replicate (maxThreads + 1) is) [0..]

        programs :: (MonadRun m) => [([Instruction], ID)] -> [m ()]
        programs insts = fmap makeProgram insts

        makeProgram :: (MonadRun m) => ([Instruction], ID) -> m ()
        makeProgram (is', n) = runReaderT (program printSteps is' >> printing Nothing printSteps) n

parseExecute :: (MonadRun m) => Bool -> Text -> Either String (m ())
parseExecute printSteps t = case parseFull t of
    (Left s) -> Left s
    (Right is) -> Right $ program' printSteps is

clearOutputs :: Int -> IO ()
clearOutputs n = sequence_ $ fmap (\(i,j) -> writeFile (outputFile i j) "") [(i, j) | i <- [0..n], j <- [0..n]]

execute :: Bool -> Text -> IO ()
execute printSteps text = do
    case parseExecute printSteps text of
        (Left l) -> putStrLn l
        (Right m) -> printCommands m >> pure ()

executeSide :: Bool -> Text -> IO ()
executeSide printSteps text = do
    clearOutputs 1
    case parseFull text of
        (Left l) -> putStrLn l
        (Right rs) -> programAll threadIt 1 printSteps rs >> pure ()
    where
        threadIt :: [ReaderT Sides IO ()] -> IO ()
        threadIt rs = do
            sides <- initSides 1
            _ <- mapConcurrently (flip runReaderT sides) rs
            pure ()

sampleProgram :: [Instruction]
sampleProgram = [
        set I (Left 31)
    , set A (Left 1)
    , mul P (Left 17)
    , jgz (Right P) (Right P)
    , mul A (Left 2)
    , add I (Left (-1))
    , jgz (Right I) (Left (-2))
    , snd (Right A)
    , rcv (Left 4)
    , snd (Left 2)
    , rcv (Left 0)
    , snd (Left 5)
    , rcv (Left 5)
    ]