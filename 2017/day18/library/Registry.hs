module Registry (
    Register(..),
    Registry,
    Value,
    zeroRegistry,
    getRegister,
    setRegister
) where

import Control.Monad.State

type Value = Either Int Register

data Register = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Show, Eq, Ord, Enum, Read)

newtype Registry = Registry [Int] deriving (Eq)

unRegister :: Registry -> [Int]
unRegister (Registry rs) = rs

zeroRegistry :: Registry
zeroRegistry = Registry $ replicate (fromEnum Z + 1) 0

getRegister :: Register -> Registry -> Int
getRegister r (Registry rs) = rs !! (fromEnum r)

setRegister :: Register -> Int -> Registry -> Registry
setRegister A val (Registry (_ : rs)) = Registry (val : rs)

setRegister reg val (Registry (r : rs)) = 
    let tailRegistry = setRegister (pred reg) val $ Registry rs
    in Registry $ r : (unRegister tailRegistry)

setRegister _ _ _ = error "setRegister failed, should never happen due to module not exposing constructors"

instance Show Registry where
    show (Registry rs) = show $ zip [A .. Z] rs