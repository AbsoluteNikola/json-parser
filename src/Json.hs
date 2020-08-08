module Json where

import Control.Applicative ((<|>))

import Parser (Parser, runParser)
import Primitives (charP, stringP)

data Json = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Double
  | JsonArray [Json]
  | JsonObject [(String, Json)]
  
jsonNullP :: Parser Json
jsonNullP = JsonNull <$ stringP "null"

jsonBoolP :: Parser Json
jsonBoolP = helper <$> (stringP "true" <|> stringP "false")
  where
    helper "true" = JsonBool True
    helper _ = JsonBool False
    -- helper will never be called without "true" or "false"
    
jsonP :: Parser Json
jsonP = jsonNullP <|> jsonBoolP

parseJson :: String -> Maybe Json
parseJson input =
  case runParser jsonP input of
    Nothing -> Nothing
    Just (_, result) -> Just result