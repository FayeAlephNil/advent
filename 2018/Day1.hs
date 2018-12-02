module Main where

getSign :: Char -> Int
getSign '+' = 1
getSign '-' = -1
getSign _ = 0

parse :: String -> Int
parse (c : r) = getSign c * read r

parseLines :: String -> [Int]
parseLines = fmap parse . lines

getScan :: String -> [Int]
getScan = scanl1 (+) . parseLines

modFreq :: String -> Int -> [Int]
modFreq s n = (+ (n * get1 s)) <$> getScan s

get1 :: String -> Int
get1 = sum . parseLines

get2 :: String -> Maybe Int
get2 s = case filter (`elem` getScan s) $ concatMap (modFreq s) [1..] of
    [] -> Nothing
    (a : _) -> Just a

part :: (Show a) => (String -> a) -> IO ()
part f = fmap f (readFile "day1.in") >>= print

main :: IO ()
main = part get2


