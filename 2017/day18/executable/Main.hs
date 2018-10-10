-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

{-# LANGUAGE NoImplicitPrelude #-}

import Prelude hiding (readFile)
import Data.Text.IO (readFile)
import Exec
import System.Environment (getArgs)


executeFile :: Bool -> String -> IO ()
executeFile printSteps path = do
    text <- readFile path
    execute printSteps text

executeFileSide :: Bool -> String -> IO ()
executeFileSide printSteps path = do
    text <- readFile path
    executeSide printSteps text

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Please provide a filename to parse"
        [fileName] -> executeFile False fileName
        [fileName, "1"] -> executeFile False fileName
        [fileName, "2"] -> executeFileSide False fileName
        [fileName, b] -> executeFile (read b) fileName
        [fileName, b, "1"] -> executeFile (read b) fileName
        [fileName, b, "2"] -> executeFileSide (read b) fileName
        _ -> putStrLn "Wrong arugments, please provide only a filename, a boolean for printing each step, and an optional number of the challenge after"
