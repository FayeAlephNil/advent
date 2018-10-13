-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

import System.Environment
import Lang

main :: IO ()
main = do
    [fileName, b] <- getArgs
    str <- readFile fileName
    let func = if (read b) then runAllSteps else runAllSteps'
    func . fmap read $ lines str

