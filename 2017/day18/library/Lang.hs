{-# LANGUAGE GADTs #-}

module Lang where

import Registry

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
