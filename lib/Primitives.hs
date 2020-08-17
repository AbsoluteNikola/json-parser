module Primitives where

import Control.Applicative (many, (<|>))
import Data.Char (isDigit)
import Data.Text as T

import Parser (Parser(..))

charP :: Char -> Parser Char
charP c = Parser f
  where
    f s = case T.uncons s of
      Nothing -> Nothing
      Just (x, xs) -> if x == c then Just (xs, x) else Nothing

digitP :: Parser Char
digitP = parseIf isDigit

stringP :: T.Text -> Parser T.Text
stringP s = T.pack <$> traverse charP (T.unpack s)

parseIf :: (Char -> Bool) -> Parser Char
parseIf p = Parser func
  where
    func s = case T.uncons s of
      Nothing -> Nothing
      Just (x, xs) -> if p x then Just (xs, x) else Nothing

spanP :: (Char -> Bool) -> Parser T.Text
spanP p =  T.pack <$> many (parseIf p)

anyCharP :: Parser Char
anyCharP = parseIf $ const True

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []
 
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  pure (x:xs)
  