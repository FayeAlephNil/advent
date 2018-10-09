{-# LANGUAGE GADTs, NoImplicitPrelude #-}

module Lang where

import Prelude hiding (mod, snd)
import qualified Prelude
import Registry
import Control.Monad.State
import Data.List.Zipper

data PureInstruction where
    SND :: Value -> PureInstruction
    SET :: Register -> Value -> PureInstruction
    ADD :: Register -> Value -> PureInstruction
    MUL :: Register -> Value -> PureInstruction
    MOD :: Register -> Value -> PureInstruction
    RCV :: Value -> PureInstruction
    deriving (Show)

data ControlInstruction where
    JGZ :: Value -> Value -> ControlInstruction
    deriving (Show)

data Instruction where
    PureInst :: PureInstruction -> Instruction
    ControlInst :: ControlInstruction -> Instruction
    deriving (Show)

snd :: Value -> Instruction
snd = PureInst . SND

set :: Register -> Value -> Instruction
set r v = PureInst $ SET r v

add :: Register -> Value -> Instruction
add r v = PureInst $ ADD r v

mul :: Register -> Value -> Instruction
mul r v = PureInst $ MUL r v

mod :: Register -> Value -> Instruction
mod r v = PureInst $ MOD r v

rcv :: Value -> Instruction
rcv = PureInst . RCV

jgz :: Value -> Value -> Instruction
jgz v1 v2= ControlInst $ JGZ v1 v2

binaryInstruction :: (MonadState (a, Registry, c) m) => (Int -> Int -> Int) -> Register -> Value -> m ()
binaryInstruction combine reg val = do
    i <- getValue val
    j <- getValue (Right reg)
    setValue reg (Left $ combine j i)

pureExec :: (MonadState (Int, Registry, [Int]) m) => PureInstruction -> m ()
pureExec (SND val) = playSound val
pureExec (SET reg val) = setValue reg val
pureExec (ADD reg val) = binaryInstruction (+) reg val
pureExec (MUL reg val) = binaryInstruction (*) reg val
pureExec (MOD reg val) = binaryInstruction (Prelude.mod) reg val
pureExec (RCV val) = do
    i <- getValue val
    if i == 0 then pure () else rcvSound

pureProgram :: (MonadState (Int, Registry, [Int]) m) => [PureInstruction] -> m ()
pureProgram = sequence_ . fmap pureExec

step :: (MonadState (Int, Registry, [Int]) m) => Zipper Instruction -> m (Maybe (Zipper Instruction))
step (Zip [] []) = pure Nothing
step (Zip _ []) = pure Nothing
step (Zip ls (PureInst p : rs)) = pureExec p >> pure (Just $ Zip (PureInst p : ls) rs)
step zipper@(Zip _ (ControlInst (JGZ xval yval) : _)) = do 
    x <- getValue xval
    y <- getValue yval
    if (x <= 0 || y == 0) then pure (Just $ right zipper) else do
        pure (Just $ (zipperFor (y) zipper))
    where
        leftFor _ (Zip [] _) = Zip [] []
        leftFor 0 zipp = zipp
        leftFor 1 zipp = left zipp
        leftFor n zipp = left $ leftFor (n - 1) zipp

        rightFor 0 zipp = zipp
        rightFor 1 zipp = right zipp
        rightFor n zipp = right $ rightFor (n - 1) zipp

        zipperFor n zipp = if n < 0 then leftFor (-n) zipp else rightFor n zipp

program :: (MonadState (Int, Registry, [Int]) m) => [Instruction] -> m ()
program is = programZipped (fromList is)
    where
        programZipped zipper = do
            cont <- step zipper
            case cont of
                Nothing -> pure ()
                (Just zipper') -> programZipped zipper'

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