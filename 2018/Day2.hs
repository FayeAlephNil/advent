{-# LANGUAGE TupleSections #-}
module Main where

import Data.Monoid
import Data.Maybe

main :: IO ()
main = part get2

part :: (Show a) => (String -> a) -> IO ()
part f = fmap f (readFile "day2.in") >>= print

get1 :: String -> Int
get1 = uncurry (*) . countNM 2 3

get2 :: String -> Maybe String
get2 s = do
    (as, bs) <- getPair s
    pure $ filterWithList (==) as bs

filterWithList :: (a -> b -> Bool) -> [a] -> [b] -> [b]
filterWithList f as bs = catMaybes $ zipWith (\a b -> if f a b then Just b else Nothing) as bs

differByOne :: (Eq a) => [a] -> [a] -> Bool
differByOne as bs = 1 == length (filterWithList (/=) as bs)

getPair :: String -> Maybe (String, String)
getPair s = listToMaybe $ do
    l <- ls
    (l,) <$> filter (differByOne l) ls
    where 
        ls = lines s

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

