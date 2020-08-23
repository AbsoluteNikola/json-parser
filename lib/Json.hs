module Json (
  module Json.Base
, module Json.Parser
, parseJson
) where

import qualified Data.Text as T

import Json.Base
import Json.Parser
import Parser

parseJson :: T.Text -> Maybe Json
parseJson input =
  case runParser jsonValueP input of
    Nothing -> Nothing
    Just (_, result) -> Just result
