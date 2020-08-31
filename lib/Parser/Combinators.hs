module Parser.Combinators (
  charP
, charIfP
, stringP
, sepBy
, sepBy1
) where

import Control.Applicative
import Control.Monad
import qualified Data.Text as T

import Parser.Base

charP :: Char -> Parser Char
charP c = do
  x <- takeAnyP
  if c == x
    then return c
    else fail . unwords $ [ "expected", show c, "got", show x ]

charIfP :: (Char -> Bool) -> Parser Char
charIfP p = do
  c <- takeAnyP
  if p c
    then return c
    else fail $ show c ++ " did not satisfy predicate"

stringP :: T.Text -> Parser T.Text
stringP s = do
  x <- T.pack <$> replicateM (T.length s) takeAnyP
  if x == s
    then return s
    else fail . unwords $ [ "expected", show s, "got", show x ]

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []
 
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  pure (x:xs)
