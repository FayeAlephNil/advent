module Parser where

import Data.Attoparsec.Text
import Lang

spinParser :: Parser (Instruction a)
spinParser = do
    undefined