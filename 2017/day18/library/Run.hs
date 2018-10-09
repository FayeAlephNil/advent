{-# LANGUAGE TupleSections #-}

module Run where

import CPU
import Control.Monad.Reader

runCommands :: ReaderT CPU IO a -> IO (a, CPU)
runCommands r = do
    cpu <- initCPU
    fmap (,cpu) $ runReaderT r cpu 

printCommands :: (Show a) => ReaderT CPU IO a -> IO (a, CPU)
printCommands r = do
    (a, cpu) <- runCommands r
    runReaderT printCPU cpu
    pure (a, cpu)
