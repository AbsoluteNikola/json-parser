module Encoder where

import qualified Data.Text as T
import Json.Base (Json(..))

string :: T.Text -> Json
string = JsonString

bool :: Bool -> Json
bool = JsonBool

double :: Double -> Json
double = JsonNumber

null :: Json
null = JsonNull

array :: (a -> Json) -> [a] -> Json
array enc arr = JsonArray $ map enc arr

object :: (a -> Json) -> [(T.Text, a)] -> Json
object enc = JsonObject . map (fmap enc)