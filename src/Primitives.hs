module Primitives where

import Control.Applicative (many)

import Parser (Parser(..))

charP :: Char -> Parser Char
charP c = Parser f
  where
    f [] = Nothing
    f (x:xs) = if x == c then Just (xs, x) else Nothing

stringP :: String -> Parser String
stringP = traverse charP

parseIf :: (Char -> Bool) -> Parser Char
parseIf p = Parser func
  where
    func [] = Nothing
    func (x:xs) = if p x then Just (xs, x) else Nothing

spanP :: (Char -> Bool) -> Parser String
spanP =  many . parseIf