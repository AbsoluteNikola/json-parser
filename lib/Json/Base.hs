module Json.Base (
  Json (..)
) where

import qualified Data.Text as T


-- Type

data Json
  = JsonNull
  | JsonBool Bool
  | JsonString T.Text
  | JsonNumber Double
  | JsonArray [Json]
  | JsonObject [(T.Text, Json)]
  deriving (Eq)

-- FIXME
instance Show Json where
  show JsonNull = "null"
  show (JsonBool True) = "true"
  show (JsonBool False) = "false"
  show (JsonString s) = show s
  show (JsonNumber x) = show x
  show (JsonArray arr) = show arr
  show (JsonObject pairs) = "{" ++ showPairs pairs ++ "}"
    where
      showPairs [] = ""
      showPairs xs = "," ++ showPair (head xs) ++ showPairs (tail xs)
      showPair (key, value) = show key ++ ":" ++ show value

-- Add Pretty here
