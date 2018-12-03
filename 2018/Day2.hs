module Main where

import Data.Monoid
import Data.Maybe

main :: IO ()
main = putStrLn "Checksum:" >> part get1 >> putStrLn "\nCommon Box:" >> part get2

part :: (Show a) => (String -> a) -> IO ()
part f = fmap f (readFile "day2.in") >>= print

get1 :: String -> Int
get1 = uncurry (*) . countNM 2 3

get2 :: String -> Maybe String
get2 s = listToMaybe $ do
    l1 <- ls
    l2 <- ls
    case foldMap maybeMon $ maybeList l1 l2 of
        (Sum 1, bs) -> pure bs
        _ -> []
    where
        ls = lines s
        maybeList = zipWith (\a b -> if a == b then Just a else Nothing)
        maybeMon Nothing = (Sum (1 :: Int), mempty)
        maybeMon (Just a) = (mempty, [a])

count :: Char -> String -> Int
count c s = length $ filter (== c) s

boolInt :: Bool -> Int
boolInt True = 1
boolInt False = 0

hasN :: Int -> String -> Bool
hasN n s = n `elem` fmap (($ s) . count) s

countNM :: Int -> Int -> String -> (Int, Int)
countNM n m s = case foldMap (\t -> (Sum $ boolInt $ hasN n t, Sum $ boolInt $ hasN m t)) $ lines s of
    (Sum a, Sum b) -> (a, b)

