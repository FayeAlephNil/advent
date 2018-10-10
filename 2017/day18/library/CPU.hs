{-# LANGUAGE TupleSections #-}

module CPU where

import Run
import Control.Monad.Reader
import Data.IORef
import Registry

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

runCommands :: ReaderT CPU IO a -> IO (a, CPU)
runCommands r = do
    cpu <- initCPU
    fmap (,cpu) $ runReaderT r cpu 

printCommands :: (Show a) => ReaderT CPU IO a -> IO (a, CPU)
printCommands r = do
    (a, cpu) <- runCommands r
    runReaderT (runReaderT printState 0) cpu
    pure (a, cpu)

instance MonadSend (ReaderT CPU IO) where
    sendInt _ i = do
        cpu <- ask
        let soundRef = cpuSound cpu
        lift $ writeIORef soundRef i

instance MonadRcv (ReaderT CPU IO) where 
    callRcv reg = do
        i <- getValue reg
        if i <= 0 then pure () else do
            cpu <- lift ask
            let (soundRef, rcvRef) = (cpuSound cpu, cpuRcv cpu)
            s <- lift . lift . readIORef $ soundRef
            lift . lift $ modifyIORef rcvRef (s:)

    getRcv = do
        cpu <- lift ask
        let rcvRef = cpuRcv cpu
        lift . lift $ readIORef rcvRef

instance MonadReg (ReaderT CPU IO) where
    setReg r i = do
        cpu <- lift ask
        lift . lift $ modifyIORef (cpuRegistry cpu) (setRegister r i)

    getReg r = do
        cpu <- lift ask
        registry <- lift . lift . readIORef . cpuRegistry $ cpu
        pure (getRegister r registry)


instance MonadRun (ReaderT CPU IO) where    
    printState = do
        cpu <- lift ask
        let (soundRef, regRef, rcvRef) = (cpuSound cpu, cpuRegistry cpu, cpuRcv cpu)
        
        sound <- lift . lift .readIORef $ soundRef
        reg <- lift . lift . readIORef $ regRef
        rcv <- lift . lift . readIORef $ rcvRef

        lift . lift . putStrLn $ "CPU State: " ++ show (sound, reg, rcv)
    printInstruction inst = do
        lift . lift . putStrLn $  "Just executed: " ++ show inst
    