module Side where

import Data.IORef
import Control.Monad.Reader
import Registry
import Run

data Side = Side {
    sideRegistry :: IORef Registry,
    sideQueue :: IORef [Int]
}

initSide :: Int -> IO Side
initSide n = do
    sideR <- newIORef (setRegister P n zeroRegistry)
    sideQ <- newIORef []
    pure Side {
        sideRegistry = sideR,
        sideQueue = sideQ
    }

type Sides = [Side]

initSides :: Int -> IO Sides
initSides maxSides = sequence . fmap initSide $ [1..maxSides]

instance MonadSend (ReaderT Sides IO) where
    sendInt n i = do
        sides <- ask
        let side = sides !! (n - 1)
        lift $ atomicModifyIORef (sideQueue side) (\is -> (i : is, ()))

instance MonadReg (ReaderT Sides IO) where
    setReg r i = do
        n <- ask
        sides <- lift ask
        let side = sides !! (n - 1)

        lift . lift $ modifyIORef (sideRegistry side) (setRegister r i)

    getReg r = do
        n <- ask
        sides <- lift ask
        let side = sides !! (n - 1)

        registry <- lift . lift . readIORef . sideRegistry $ side
        pure (getRegister r registry)

instance MonadRcv (ReaderT Sides IO) where 
    callRcv (Right r) = do
        n <- ask
        sides <- lift ask
        let side = sides !! (n - 1)
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
        let queueRef = sideQueue (sides !! (n - 1))
        lift . lift $ readIORef queueRef
        

instance MonadRun (ReaderT Sides IO) where    
    printState = do
        j <- ask
        sides <- lift ask
        _ <- lift . lift . sequence . fmap (\(side, i) -> printSide i j side) $ zip sides [1..]
        pure ()

outputFile :: Int -> Int -> String
outputFile i j = "output/" ++ show i ++ "-" ++ show j

printSide :: Int -> Int -> Side -> IO ()
printSide i j side = do
    registry <- readIORef (sideRegistry side)
    queue <- readIORef (sideQueue side)
    appendFile (outputFile i j) $ "\nInfo for side " ++ show i ++ " from side " ++ show j ++ ": " ++ show (registry, queue)