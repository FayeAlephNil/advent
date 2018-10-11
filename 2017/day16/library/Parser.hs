{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Attoparsec.Text
import Control.Applicative
import Data.Char
import Lang

spinParser :: Parser (Instruction a)
spinParser = do
    _ <- char 's'
    i <- decimal
    pure $ Spin i

exchParser :: Parser (Instruction a)
exchParser = do
    _ <- char 'x'
    i <- decimal
    _ <- char '/'
    j <- decimal
    pure $ Swap i j

partnerParser :: Parser (Instruction Char)
partnerParser = do
    _ <- char 'p'
    c1 <- satisfy isLetter
    _ <- char '/'
    c2 <- satisfy isLetter
    pure $ Partner c1 c2

instParser :: Parser (Instruction Char)
instParser = spinParser <|> exchParser <|> partnerParser

fullParser :: Parser ([Instruction Char])
fullParser = instParser `sepBy` (char ',')