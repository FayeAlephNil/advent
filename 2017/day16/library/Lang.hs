module Lang where

data Instruction a = Spin Int | Swap Int Int | Partner a a
    deriving (Show, Eq)

type Instructions a = [Instruction a]