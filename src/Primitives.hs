module Primitives where

import Control.Applicative (many, (<|>))
import Data.Char (isDigit)

import Parser (Parser(..))

charP :: Char -> Parser Char
charP c = Parser f
  where
    f [] = Nothing
    f (x:xs) = if x == c then Just (xs, x) else Nothing

digitP :: Parser Char
digitP = parseIf isDigit

stringP :: String -> Parser String
stringP = traverse charP

parseIf :: (Char -> Bool) -> Parser Char
parseIf p = Parser func
  where
    func [] = Nothing
    func (x:xs) = if p x then Just (xs, x) else Nothing

spanP :: (Char -> Bool) -> Parser String
spanP =  many . parseIf

anyCharP :: Parser Char
anyCharP = parseIf $ const True

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []
 
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  pure (x:xs)
  