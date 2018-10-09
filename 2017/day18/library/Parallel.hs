module Parallel where

import Data.IORef
import Control.Monad.Reader
import Registry
import Run

data Side = Side {
    sideRegistry :: IORef Registry,
    sideQueue :: IORef [Int]
}

type Sides = [Side]

instance MonadSend (ReaderT Sides IO) where
    sendInt n i = do
        sides <- ask
        let side = sides !! n
        lift $ atomicModifyIORef (sideQueue side) (\is -> (i : is, ()))

instance MonadReg (ReaderT Sides IO) where
    setReg r i = do
        n <- ask
        sides <- lift ask
        let side = sides !! n

        lift . lift $ modifyIORef (sideRegistry side) (setRegister r i)

    getReg r = do
        n <- ask
        sides <- lift ask
        let side = sides !! n

        registry <- lift . lift . readIORef . sideRegistry $ side
        pure (getRegister r registry)

instance MonadRcv (ReaderT Sides IO) where 
    callRcv (Right r) = do
        n <- ask
        sides <- lift ask
        let side = sides !! n
        let queueRef = sideQueue side
        queue <- lift . lift . readIORef $ queueRef
        case queue of
            [] -> callRcv (Right r)
            (a : as) -> do
                lift . lift $ atomicWriteIORef queueRef as
                setReg r a
        
    callRcv (Left _) = lift . lift . putStrLn $ "Execution Error: Used a number literal with rcv"

    getRcv =  do
        n <- ask
        sides <- lift ask
        let queueRef = sideQueue (sides !! n)
        lift . lift $ readIORef queueRef
        

instance MonadRun (ReaderT Sides IO) where    
    printState = do
        i <- ask
        sides <- lift ask
        _ <- lift . lift . sequence . fmap (printSide i) $ sides
        pure ()
    
printSide :: Int -> Side -> IO ()
printSide i side = do
    registry <- readIORef (sideRegistry side)
    queue <- readIORef (sideQueue side)
    print $ "Info for side " ++ show i ++ ": " ++ show (registry, queue)