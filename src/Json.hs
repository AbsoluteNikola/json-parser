module Json where

import Control.Applicative ((<|>), many, some)
import Data.Char (isSpace)

import Parser (Parser, runParser)
import Primitives

data Json 
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Double
  | JsonArray [Json]
  | JsonObject [(String, Json)]
  deriving (Show, Eq)

wsP :: Parser String
wsP = spanP isSpace

jsonNullP :: Parser Json
jsonNullP = JsonNull <$ stringP "null"

jsonBoolP :: Parser Json
jsonBoolP = helper <$> (stringP "true" <|> stringP "false")
  where
    helper "true" = JsonBool True
    helper _ = JsonBool False
    -- helper will never be called without "true" or "false"


intP :: Parser Double
intP = (read <$> stringP "0") <|> (read <$> intPart)
  where
    intPart = (:) <$> parseIf (`elem` ['1'..'9']) <*> many digitP

floatP :: Parser Double
floatP = (charP '.' *> floatingPart) <|> pure 0
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

jsonNumberP :: Parser Json
jsonNumberP = 
  fmap JsonNumber $
    toDouble 
       <$> signP 
       <*> intP
       <*> (floatP <|> pure 0)
       <*> (expP <|> pure 0)
  where
    signP = ((-1) <$ charP '-') <|> pure 1
    toDouble numSign int float e = numSign * (int + float) * (10 ** e)


jsonP :: Parser Json
jsonP = jsonNullP <|> jsonBoolP <|> jsonNumberP

parseJson :: String -> Maybe Json
parseJson input =
  case runParser jsonP input of
    Nothing -> Nothing
    Just (_, result) -> Just result