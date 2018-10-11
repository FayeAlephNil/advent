module Main where

import Data.Attoparsec.Text
import qualified Data.Text.IO as TextIO
import Control.Monad
import Data.Char
import System.Environment

main :: IO ()
main = do
    [filename, n] <- getArgs
    text <- TextIO.readFile filename
    let f = if (read n) == 1 then Just . range else rangeDivis
    case parseOnly (fmap (consumeSpreadsheetWith f) parser) text of
        (Left l) -> error l
        (Right Nothing) -> error "No divisibles on some lines"
        (Right (Just r)) -> print r

parser :: Parser [[Int]]
parser = lineParse `sepBy1'` endOfLine
    where
        lineParse = decimal `sepBy'` skipWhile cond
        cond c = isSpace c && not (isEndOfLine c)

range :: [Int] -> Int
range xs = maximum xs - minimum xs

findDivis :: [Int] -> [(Int, Int)]
findDivis xs = [(a, b) | a <- xs, b <- xs, a `mod` b == 0, a /= b]

rangeDivis :: [Int] -> Maybe Int
rangeDivis = safeHead . fmap (uncurry div) . findDivis
    where
        safeHead [] = Nothing
        safeHead (a : _) = Just a

consumeSpreadsheetWith :: ([Int] -> Maybe Int) -> [[Int]] -> Maybe Int
consumeSpreadsheetWith f = fmap sum . sequence . fmap f