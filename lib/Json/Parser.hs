module Json.Parser (
  jsonValueP
, jsonObjectP
, jsonArrayP
, jsonNumberP
, jsonStringP
, jsonNullP
) where

import Control.Applicative ((<|>), many, some)
import Data.Char (isSpace, isDigit)
import qualified Data.Text as T

import Json.Base
import Parser

-- Parsing

wsP :: Parser T.Text
wsP = takeWhileP isSpace

digitP :: Parser Char
digitP = charIfP isDigit

jsonNullP :: Parser Json
jsonNullP = JsonNull <$ stringP "null"

jsonBoolP :: Parser Bool
jsonBoolP = trueP <|> falseP
  where
    trueP = stringP "true" >> pure True
    falseP = stringP "false" >> pure False

intP :: Parser Double
intP = (read . T.unpack <$> stringP "0") <|> (read <$> intPart)
  where
    intPart = (:) <$> charIfP (`elem` ['1'..'9']) <*> many digitP

floatP :: Parser Double
floatP = charP '.' *> floatingPart
  where
    floatingPart = read . ("0."++) <$> some digitP

expP :: Parser Double
expP =
  applySign
    <$ (charP 'e' <|> charP 'E')
    <*> (((-1) <$ charP '-') <|> (1 <$ charP '+') <|> pure 1)
    <*> (read <$> some digitP)
  where
    applySign sign number = sign * number

jsonNumberP :: Parser Double
jsonNumberP =
  toDouble
    <$> signP
    <*> intP
    <*> (floatP <|> pure 0)
    <*> (expP <|> pure 0)
  where
    signP = ((-1) <$ charP '-') <|> pure 1
    toDouble numSign int float e = numSign * (int + float) * (10 ** e)

escapeP :: Parser Char
escapeP = do
  _ <- charP '\\'
  c <- takeAnyP
  maybe (fail "") pure (lookup c escapeCharacters)
  where
    escapeCharacters =
      [ ('"', '"')
      , ('\\', '\\')
      , ('/', '/')
      , ('b', '\b')
      , ('f', '\f')
      , ('n', '\n')
      , ('r', '\r')
      , ('t', '\t')
      ]

jsonStringP :: Parser T.Text
jsonStringP = do
  _ <- charP '"'
  str <- T.pack <$> many (escapeP <|> charIfP (/= '"'))
  _ <- charP '"'
  pure str

jsonValueP :: Parser Json
jsonValueP = do
  _ <- wsP
  res <- jsonNullP
      <|> (JsonBool <$> jsonBoolP)
      <|> (JsonNumber <$> jsonNumberP)
      <|> (JsonString <$> jsonStringP)
      <|> jsonArrayP
      <|> jsonObjectP
  _ <- wsP
  pure res

jsonArrayP :: Parser Json
jsonArrayP = do
  _ <- charP '['
  _ <- wsP
  values <- sepBy jsonValueP (charP ',')
  _ <- charP ']'
  pure $ JsonArray values

jsonObjectP :: Parser Json
jsonObjectP = do
  _ <- charP '{'
  _ <- wsP
  values <- sepBy pairP (charP ',')
  _ <- charP '}'
  pure $ JsonObject values
  where
    pairP = do
      _ <- wsP
      key <- jsonStringP
      _ <- wsP
      _ <- charP ':'
      value <- jsonValueP
      pure (key, value)