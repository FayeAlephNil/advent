{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Attoparsec.Text
import qualified Data.Text as Text
import Data.Monoid
import Data.List
import Data.Maybe

data Point = Point Int Int deriving (Read, Show, Eq, Ord)
type ID = Int
data Claim = Claim ID Rect deriving (Read, Show)
type FabricID = Map Point ClaimInfo
data Rect = Rect Point Int Int deriving (Read, Show)
data ClaimInfo = NotClaimed | Claimed ID | Conflict deriving (Read, Show, Eq)

getClaim :: ClaimInfo -> Maybe Int
getClaim (Claimed i) = Just i
getClaim _ = Nothing

maybeTup :: (a, Maybe b) -> Maybe (a, b)
maybeTup (_, Nothing) = Nothing
maybeTup (a, Just b) = Just (a, b)

main :: IO ()
main = part get2

part :: (Show a) => ([Claim] -> FabricID -> a) -> IO ()
part f = do 
    mapFile <- readFile "day3.map"
    claims <- readFile "day3.in" >>= fullParse
    themap <- case mapFile of
        "" -> let 
            themap' = getMap <$> claims
            in writeFile "day3.map" (show themap') >> pure themap'
        _ -> pure $ read mapFile
    print $ f <$> claims <*> themap
    where
        fullParse = pure . traverse (parseOnly parseClaim . Text.pack) . lines

get1 :: [Claim] -> FabricID -> Int
get1 = const (length . Map.toList . Map.filter (== Conflict))

-- get2 :: [Claim] -> Int
get2 cs fullmap = let
    getOne i ci = maybeTup (i, getClaim ci)
    oneClaimMap = catMaybes $ uncurry getOne <$> Map.toList fullmap
    ids = fmap snd oneClaimMap
    givenId id' = fst <$> filter ((== id') . snd) oneClaimMap
    simpleClaims = fmap simpleClaim cs
    oneClaims = fmap (\id' -> (id', givenId id')) ids
    in fst $ head $ intersect oneClaims simpleClaims

getMap :: [Claim] -> FabricID
getMap claims = appEndo (foldMap (Endo . addClaim) claims) empty

parseClaim :: Parser Claim
parseClaim = do
    char '#'
    id' <- decimal
    string " @ "
    x <- decimal
    char ','
    y <- decimal
    string ": "
    w <- decimal
    char 'x'
    t <- decimal
    pure $ Claim id' (Rect (Point x y) w t)

fabric :: [Point]
fabric = [Point x y | x <- [0..999], y <- [0..999]]

empty :: FabricID
empty = Map.fromList $ fmap (,NotClaimed) fabric

between :: Int -> Int -> Int -> Bool
between a b c = (a <= b) && (b < c)

-- Tells if first point is within the given rectangle
within :: Point -> Rect -> Bool
within (Point px py) (Rect (Point qx qy) w t) = between qx px (qx + w) && between qy py (qy + t)

simpleClaim :: Claim -> (ID, [Point])
simpleClaim (Claim id' rect) = (id', filter (`within` rect) fabric)

addClaim :: Claim -> FabricID -> FabricID
addClaim (Claim id' rect) = Map.mapWithKey go
    where
        go :: Point -> ClaimInfo -> ClaimInfo
        go q cinfo= if q `within` rect then ifWithin cinfo id' else cinfo
        go q Conflict = Conflict

        ifWithin NotClaimed i = Claimed i
        ifWithin (Claimed j) _ = Conflict
        ifWithin Conflict _ = Conflict
