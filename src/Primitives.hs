module Primitives where

import Parser (Parser(..))

charP :: Char -> Parser Char
charP c = Parser f
  where
    f [] = Nothing
    f (x:xs) = if x == c then Just (xs, x) else Nothing