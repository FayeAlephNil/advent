module Main where

import System.IO

getSign :: Char -> Int
getSign '+' = 1
getSign '-' = -1
getSign _ = 0

parse :: String -> Int
parse (c : r) = getSign c * read r

parseLines :: String -> [Int]
parseLines = fmap parse . lines

parseCycle :: String -> [Int]
parseCycle = fmap parse . cycle . lines

get1 :: String -> Int
get1 = sum . parseLines

firstRepeat :: (Eq a) => [a] -> Maybe a
firstRepeat as = go (as, [])
    where
        go :: (Eq a) => ([a], [a]) -> Maybe a
        go ([], _) = Nothing
        go (b : bs, seen) = if b `elem` seen then Just b else go (bs, b : seen)

get2 :: String -> Maybe Int
get2 = firstRepeat . scanl (+) 0 . parseCycle

part :: (Show a) => (String -> a) -> IO ()
part f = fmap f (readFile "day1.in") >>= print

main = part get2


