module Json where

data Json = JsonNull
  | JsonBool Bool 
  | JsonString String
  | JsonNumber Double
  | JsonArray [Json]
  | JsonObject [(String, Json)]
