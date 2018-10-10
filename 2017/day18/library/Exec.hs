module Exec where

import Prelude hiding (readFile)
import Parsing (parseExecute, parseFull)
import CPU (printCommands)
import Lang (programAll)
import Data.Text
import Control.Monad.Reader
import Control.Concurrent.Async
import Side

clearOutputs :: Int -> IO ()
clearOutputs n = sequence_ $ fmap (\(i,j) -> writeFile (outputFile i j) "") [(i, j) | i <- [1..n], j <- [1..n]]


execute :: Bool -> Text -> IO ()
execute printSteps text = do
    case parseExecute printSteps text of
        (Left l) -> putStrLn l
        (Right m) -> printCommands m >> pure ()

executeSide :: Bool -> Text -> IO ()
executeSide printSteps text = do
    clearOutputs 2
    case parseFull text of
        (Left l) -> putStrLn l
        (Right rs) -> programAll threadIt 2 printSteps rs >> pure ()
    where
        threadIt :: [ReaderT Sides IO ()] -> IO ()
        threadIt rs = do
            sides <- initSides 2
            _ <- mapConcurrently (flip runReaderT sides) rs
            pure ()