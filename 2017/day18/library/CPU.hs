{-# LANGUAGE TupleSections #-}

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

printCPU :: CPU -> IO ()
printCPU (CPU soundRef regRef rcvRef) = do
    sound <- readIORef soundRef
    reg <- readIORef regRef
    rcv <- readIORef rcvRef
    print (sound, reg, rcv)

runCommands :: ReaderT CPU IO a -> IO (a, CPU)
runCommands r = do
    cpu <- initCPU
    fmap (,cpu) $ runReaderT r cpu 

printCommands :: (Show a) => ReaderT CPU IO a -> IO (a, CPU)
printCommands r = do
    (a, cpu) <- runCommands r
    print a
    printCPU cpu
    pure (a, cpu)

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

