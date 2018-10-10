{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Parsing where

import Prelude hiding (mod, snd, lines)

import Data.Attoparsec.Text
import Data.Text hiding (length, zip, filter, toUpper)
import Data.Char
import Data.Either (rights)

import Control.Applicative

import Lang
import Registry

parseFull :: Text -> Either String [Instruction]
parseFull t = case (filter condition $ zip theParse [(0 ::Int)..]) of
    [] -> Right (rights theParse)
    ((Left s, i) : _) -> Left ("Parsing error at line " ++ show i ++ ": " ++ s)
    _ -> error "Should never reach here"
    where
        theParse = parseFull' t

        condition (Left _, _) = True
        condition _ = False

parseFull' :: Text -> [Either String Instruction]
parseFull' t = fmap (parseOnly lineParser) $ lines t

lineParser :: Parser Instruction
lineParser = sndParser
         <|> setParser
         <|> addParser
         <|> mulParser
         <|> modParser
         <|> rcvParser
         <|> jgzParser
         <|> (skipSpace >> pure (rcv (Left 0)))

digits :: [Int] -> Int
digits xs = sum . fmap (\(x, n) -> x * 10 ^ n) $ zip xs [(length xs - 1), (length xs - 2)..0]

registerParser :: Parser Register
registerParser = fmap (read . (:[]) . toUpper) $ satisfy isLower

valueParser :: Parser Value
valueParser = (fmap Left (signed decimal)) <|> (fmap Right registerParser)

sndParser :: Parser Instruction
sndParser = do
    _ <- string "snd"
    skipSpace
    val <- valueParser
    pure $ snd val

setParser :: Parser Instruction
setParser = do
    _ <- string "set"
    skipSpace
    reg <- registerParser
    skipSpace
    val <- valueParser
    pure $ set reg val

addParser :: Parser Instruction
addParser = do
    _ <- string "add"
    skipSpace
    reg <- registerParser
    skipSpace
    val <- valueParser
    pure $ add reg val

mulParser :: Parser Instruction
mulParser = do
    _ <- string "mul"
    skipSpace
    reg <- registerParser
    skipSpace
    val <- valueParser
    pure $ mul reg val

modParser :: Parser Instruction
modParser = do
    _ <- string "mod"
    skipSpace
    reg <- registerParser
    skipSpace
    val <- valueParser
    pure $ mod reg val

rcvParser :: Parser Instruction
rcvParser = do
    _ <- string "rcv"
    skipSpace
    val <- valueParser
    pure $ rcv val

jgzParser :: Parser Instruction
jgzParser = do
    _ <- string "jgz"
    skipSpace
    xval <- valueParser
    skipSpace
    yval <- valueParser
    pure $ jgz xval yval