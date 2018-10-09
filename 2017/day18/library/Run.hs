module Run where

import Registry
import Control.Monad.Reader

type ID = Int 

class (Monad m) => MonadSend m where
    sendInt :: Int -- Who we're sending to
            -> Int -> m ()

class (Monad m) => MonadReg m where
    setReg :: Register -> Int -> ReaderT ID m ()
    getReg :: Register -> ReaderT ID m Int

class (Monad m) => MonadRcv m where
    callRcv :: Value -> ReaderT ID m ()
    getRcv :: ReaderT ID m [Int]

class (MonadSend m, MonadRcv m, MonadReg m) => MonadRun m where
    printState :: ReaderT ID m ()

getValue :: (MonadReg m) => Value -> ReaderT ID m Int
getValue (Left i) = pure i
getValue (Right r) = getReg r

setValue :: (MonadReg m) => Register -> Value -> ReaderT ID m ()
setValue r val = do
    i <- getValue val
    setReg r i

send :: (MonadReg m, MonadSend m) => ID -> Value -> ReaderT ID m ()
send n val = do
    i <- getValue val
    lift $ sendInt n i

