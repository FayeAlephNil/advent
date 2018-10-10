module Side where

import Data.IORef
import Control.Monad.Reader
import Registry
import Run
import GHC.Conc.Sync

data Side = Side {
    sideRegistry :: IORef Registry,
    sideQueue :: TVar [Int]
}

initSide :: Int -> IO Side
initSide n = do
    sideR <- newIORef (setRegister P n zeroRegistry)
    sideQ <- atomically . newTVar $ []
    pure Side {
        sideRegistry = sideR,
        sideQueue = sideQ 
    }

type Sides = [Side]

initSides :: Int -> IO Sides
initSides maxSides = sequence . fmap initSide $ [0..maxSides]

getQueueContents :: Side -> IO [Int]
getQueueContents side = atomically . readTVar $ sideQueue side
    

instance MonadSend (ReaderT Sides IO) where
    sendInt n i = do
        sides <- ask
        let side = sides !! n
        let queueVar = sideQueue side
        queue <- lift . atomically . readTVar $ queueVar
        lift . atomically . writeTVar queueVar $ (i : queue)

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
        rcvd <- lift . lift . atomically $ do
            let queueVar = sideQueue side
            queue <- readTVar queueVar
            case queue of
                [] -> retry
                (q : queue') -> writeTVar queueVar queue' >> pure q
        setReg r rcvd
        
    callRcv (Left _) = lift . lift . putStrLn $ "Execution Error: Used a number literal with rcv"

    getRcv =  do
        n <- ask
        sides <- lift ask
        lift . lift . getQueueContents $ sides !! n
        

instance MonadRun (ReaderT Sides IO) where    
    printState = do
        j <- ask
        sides <- lift ask
        _ <- lift . lift . sequence . fmap (\(side, i) -> printSide i j side) $ zip sides [0..]
        pure ()
    
    printInstruction inst = do
        i <- ask
        lift . lift . appendFile (outputFile i i) $ "\nJust executed: " ++ show inst

outputFile :: Int -> Int -> String
outputFile i j = "output/" ++ show i ++ "-" ++ show j ++ ".out"

printSide :: Int -> Int -> Side -> IO ()
printSide i j side = do
    registry <- readIORef (sideRegistry side)
    queue <- getQueueContents side
    appendFile (outputFile i j) $ "\nInfo for side " ++ show i ++ " from side " ++ show j ++ ": " ++ show (registry, queue)