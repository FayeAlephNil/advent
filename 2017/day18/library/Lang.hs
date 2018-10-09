{-# LANGUAGE GADTs, NoImplicitPrelude #-}

module Lang where

import Prelude hiding (mod, snd)
import qualified Prelude
import Registry
import Run
import Control.Monad.Reader
import Data.List.Zipper

data RegInstruction where
    SET :: Register -> Value -> RegInstruction
    ADD :: Register -> Value -> RegInstruction
    MUL :: Register -> Value -> RegInstruction
    MOD :: Register -> Value -> RegInstruction
    deriving (Show)

data ChannelInstruction where
    SND :: Value -> ChannelInstruction
    RCV :: Value -> ChannelInstruction
    deriving (Show)

data ControlInstruction where
    JGZ :: Value -> Value -> ControlInstruction
    deriving (Show)

data Instruction where
    RegInst :: RegInstruction -> Instruction
    ChannelInst :: ChannelInstruction -> Instruction
    ControlInst :: ControlInstruction -> Instruction
    deriving (Show)

snd :: Value -> Instruction
snd = ChannelInst . SND

set :: Register -> Value -> Instruction
set r v = RegInst $ SET r v

add :: Register -> Value -> Instruction
add r v = RegInst $ ADD r v

mul :: Register -> Value -> Instruction
mul r v = RegInst $ MUL r v

mod :: Register -> Value -> Instruction
mod r v = RegInst $ MOD r v

rcv :: Value -> Instruction
rcv = ChannelInst . RCV

jgz :: Value -> Value -> Instruction
jgz v1 v2= ControlInst $ JGZ v1 v2

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

printing :: (MonadRun m) => Bool -> ReaderT ID m ()
printing weDo = if weDo then printState else pure ()

step :: (MonadRun m) => Bool -> Zipper Instruction -> ReaderT ID m (Maybe (Zipper Instruction))
step _ (Zip [] []) = pure Nothing
step _ (Zip _ []) = pure Nothing
step printSteps (Zip ls (ChannelInst ci : rs)) = do
    channelExec ci
    printing printSteps
    pure (Just $ Zip (ChannelInst ci : ls) rs)
step printSteps (Zip ls (RegInst p : rs)) = do
    regExec p
    printing printSteps
    pure (Just $ Zip (RegInst p : ls) rs)
step printSteps zipper@(Zip _ (ControlInst ci : _)) = do 
    zipper' <- controlExec ci zipper
    printing printSteps
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

programAll :: (MonadRun m) => Bool -> [Instruction] -> m ()
programAll = undefined

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