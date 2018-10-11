-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.

import Line
import Parser
import Exec
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as Atto
import System.Environment

-- For Part 2, notice where it repeats (every 0 ~ 300 so 100 ~ 1 billion = 300 * 33 + 100)
main :: IO ()
main = do
    [n] <- getArgs
    t <- T.readFile "input"
    let ln = lineFromList "abcdefghijklmnop"
    case Atto.parseOnly fullParser t of
        Left s -> error s
        Right is -> print $ execAllWith (take (length is * read n) . cycle $ is) ln
