module Json where

import Control.Applicative ((<|>), many, some)
import Data.Char (isSpace, isDigit)

import Parser (Parser, runParser)
import Primitives

data Json = JsonNull
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

jsonNumberP :: Parser Json
jsonNumberP = JsonNumber <$> (
              toDouble <$> signP <*> intP
                       <*> (floatP <|> pure 0)
                       <*> (expP <|> pure 0))
  where
    digit = parseIf isDigit
    signP = ((-1) <$ charP '-') <|> pure 1
    -- | 0. or 1012.
    intP :: Parser Double
    intP = (read <$> stringP "0") <|> (read <$> intP')
    intP' = (:) <$> parseIf (`elem` ['1'..'9']) <*> many digit
    -- | parse .123 into Double or return 0
    floatP :: Parser Double
    floatP = (charP '.' *> floatP') <|> pure 0
    floatP' = read . ("0."++) <$> some digit
    expP = expHelper <$ (charP 'e' <|> charP 'E')
                     <*> (((-1) <$ charP '-') <|> pure 1)
                     <*> some digit
    expHelper :: Double -> String -> Double
    expHelper sign number = sign * read number
    toDouble :: Double -> Double -> Double -> Double -> Double
    toDouble numSign int float e = numSign * (int + float) * (10 ** e)


jsonP :: Parser Json
jsonP = jsonNullP <|> jsonBoolP <|> jsonNumberP

parseJson :: String -> Maybe Json
parseJson input =
  case runParser jsonP input of
    Nothing -> Nothing
    Just (_, result) -> Just result