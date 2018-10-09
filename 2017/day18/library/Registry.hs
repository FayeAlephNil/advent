module Registry (
    Register(..),
    Registry,
    Value,
    zeroRegistry,
    getRegister,
    setRegister,
    getValue,
    setValue,
    playSound,
    getSound,
    getRegistry,
    rcvSound
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

getFst :: (MonadState (a, b, c) m) => m a
getFst = do
    (a, _, _) <- get
    pure a

getSnd :: (MonadState (a, b, c) m) => m b
getSnd = do
    (_, b, _) <- get
    pure b

getThrd :: (MonadState (a, b, c) m) => m c
getThrd = do
    (_, _, c) <- get
    pure c

modifySnd :: (MonadState (a, b, c) m) => (b -> b) -> m ()
modifySnd f = do
    (a, b, c) <- get
    put (a, f b, c)

getSound :: (MonadState (Int, Registry, [Int]) m) => m Int
getSound = getFst

getRegistry :: (MonadState (Int, Registry, [Int]) m) => m Registry
getRegistry = getSnd

getRcv :: (MonadState (Int, Registry, [Int]) m) => m [Int]
getRcv = getThrd

getValue :: (MonadState (a, Registry, b) m) => Value -> m Int
getValue (Left n) = pure n
getValue (Right r) = do
    reg <- getSnd
    pure $ getRegister r reg

setValue :: (MonadState (a, Registry, b) m) => Register -> Value -> m ()
setValue r val = do
    i <- getValue val
    modifySnd (setRegister r i)

playSound :: (MonadState (Int, Registry, [Int]) m) => Value -> m ()
playSound val = do
    i <- getValue val
    (_, reg, c) <- get
    put (i, reg, c)

rcvSound :: (MonadState (Int, Registry, [Int]) m) => m ()
rcvSound = do
    (a, b, rcvs) <- get
    put (a, b, a:rcvs)