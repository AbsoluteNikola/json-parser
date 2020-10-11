{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module QQ (json)
where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Exp(..), Q(..))
import qualified Data.Text as T

import Json (parseJson, Json(..))

json :: QuasiQuoter
json = QuasiQuoter
  { quoteExp = parse
  , quotePat = unsupported
  , quoteType = unsupported
  , quoteDec = unsupported
  }
  where
    unsupported _ = fail "json QuasiQuoter can not be used in this context"
    parse str =
      case parseJson (T.pack str) of
        Just val -> toExp val
        Nothing -> fail "Incorrect json"

toExp :: Json -> Q Exp
toExp JsonNull = [| JsonNull |]
toExp (JsonBool b) = [| JsonBool b |]
toExp (JsonString s) = [| JsonString s |]
toExp (JsonNumber x) = [| JsonNumber x |]
toExp (JsonArray arr) = [| JsonArray $(ListE <$> mapM toExp arr) |]
toExp (JsonObject obj) = [| JsonObject $pairs |]
  where
    helper (key, val) = [| (key, $(toExp val)) |]
    pairs = ListE <$> mapM helper obj
