module CPU where

import Data.IORef
import Registry
import Control.Monad.State
import Control.Monad.Reader

data CPU = CPU {
    cpuSound :: IORef Int,
    cpuRegistry :: IORef Registry,
    cpuRcv :: IORef [Int]
} deriving (Eq)

initCPU :: IO CPU
initCPU = do
    soundRef <- newIORef 0
    regRef <- newIORef zeroRegistry
    rcvRef <- newIORef []
    pure $ CPU {
        cpuSound = soundRef,
        cpuRegistry = regRef,
        cpuRcv = rcvRef
    }

class (Monad m) => MonadSend m where
    sendInt :: Int -> m ()

class (Monad m) => MonadRcv m where
    callRcv :: m ()
    getRcv :: m [Int]

class (Monad m) => MonadReg m where
    setReg :: Register -> Int -> m ()
    getReg :: Register -> m Int

class (MonadSend m, MonadRcv m, MonadReg m) => MonadCPU m where
    printCPU :: m ()

instance MonadSend (ReaderT CPU IO) where
    sendInt i = do
        cpu <- ask
        let soundRef = cpuSound cpu
        lift $ writeIORef soundRef i

instance MonadRcv (ReaderT CPU IO) where 
    callRcv = do
        cpu <- ask
        let (soundRef, rcvRef) = (cpuSound cpu, cpuRcv cpu)
        s <- lift . readIORef $ soundRef
        lift $ modifyIORef rcvRef (s:)

    getRcv = do
        cpu <- ask
        let rcvRef = cpuRcv cpu
        lift $ readIORef rcvRef

instance MonadReg (ReaderT CPU IO) where
    setReg r i = do
        cpu <- ask
        lift $ modifyIORef (cpuRegistry cpu) (setRegister r i)

    getReg r = do
        cpu <- ask
        registry <- lift . readIORef . cpuRegistry $ cpu
        pure (getRegister r registry)


instance MonadCPU (ReaderT CPU IO) where    
    printCPU = do
        cpu <- ask
        let (soundRef, regRef, rcvRef) = (cpuSound cpu, cpuRegistry cpu, cpuRcv cpu)
        
        sound <- lift . readIORef $ soundRef
        reg <- lift . readIORef $ regRef
        rcv <- lift . readIORef $ rcvRef

        lift . putStrLn $ "CPU State: " ++ show (sound, reg, rcv)

getValue :: (MonadReg m) => Value -> m Int
getValue (Left i) = pure i
getValue (Right r) = getReg r

setValue :: (MonadReg m) => Register -> Value -> m ()
setValue r val = do
    i <- getValue val
    setReg r i

send :: (MonadReg m, MonadSend m) => Value -> m ()
send val = do
    i <- getValue val
    sendInt i

instance {-# OVERLAPPING #-} MonadState (Int, Registry, [Int]) (ReaderT CPU IO) where
    state f = do
        cpu <- ask
        let (soundRef, regRef, rcvRef) = (cpuSound cpu, cpuRegistry cpu, cpuRcv cpu)
        
        sound <- lift . readIORef $ soundRef
        reg <- lift . readIORef $ regRef
        rcv <- lift . readIORef $ rcvRef

        let (a, (sound', reg', rcv')) = f (sound, reg, rcv)
        lift $ writeIORef soundRef sound'
        lift $ writeIORef regRef reg'
        lift $ writeIORef rcvRef rcv'
        
        pure a

