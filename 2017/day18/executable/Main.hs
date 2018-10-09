-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

{-# LANGUAGE NoImplicitPrelude #-}

import Prelude hiding (readFile)
import Parsing (parseExecute)
import CPU (printCommands)
import System.Environment (getArgs)
import Data.Text.IO (readFile)

executeFile :: String -> IO ()
executeFile path = do
    text <- readFile path
    case parseExecute text of
        (Left l) -> putStrLn l
        (Right m) -> printCommands m >> pure ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Please provide a filename to parse"
        [fileName] -> executeFile fileName
        _ -> putStrLn "Too many arguments, please provide only a filename"
