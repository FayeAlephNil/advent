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

class (Monad m) => MonadCPU m where
    putSound :: Int -> m ()
    getSound :: m Int

    callRcv :: m ()
    getRcv :: m [Int]

    setReg :: Register -> Int -> m ()
    getReg :: Register -> m Int

    printCPU :: m ()

instance MonadCPU (ReaderT CPU IO) where
    putSound i = do
        cpu <- ask
        let soundRef = cpuSound cpu
        lift $ writeIORef soundRef i
    
    getSound = do
        cpu <- ask
        lift . readIORef . cpuSound $ cpu

    callRcv = do
        cpu <- ask
        let (soundRef, rcvRef) = (cpuSound cpu, cpuRcv cpu)
        s <- lift . readIORef $ soundRef
        lift $ modifyIORef rcvRef (s:)

    getRcv = do
        cpu <- ask
        let rcvRef = cpuRcv cpu
        lift $ readIORef rcvRef

    setReg r i = do
        cpu <- ask
        lift $ modifyIORef (cpuRegistry cpu) (setRegister r i)

    getReg r = do
        cpu <- ask
        registry <- lift . readIORef . cpuRegistry $ cpu
        pure (getRegister r registry)

    printCPU = do
        cpu <- ask
        let (soundRef, regRef, rcvRef) = (cpuSound cpu, cpuRegistry cpu, cpuRcv cpu)
        
        sound <- lift . readIORef $ soundRef
        reg <- lift . readIORef $ regRef
        rcv <- lift . readIORef $ rcvRef

        lift . putStrLn $ "CPU State: " ++ show (sound, reg, rcv)

getValue :: (MonadCPU m) => Value -> m Int
getValue (Left i) = pure i
getValue (Right r) = getReg r

setValue :: (MonadCPU m) => Register -> Value -> m ()
setValue r val = do
    i <- getValue val
    setReg r i

playSound :: (MonadCPU m) => Value -> m ()
playSound val = do
    i <- getValue val
    putSound i

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

