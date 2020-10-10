module TestEncoder where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Encoder as E
import Json (Json(..))

tests :: TestTree
tests = testGroup "Encoder"
  [ testCase "string" $
      E.string "aa" @?= JsonString "aa"
  , testCase "bool" $
      E.bool True @?= JsonBool True
  , testCase "number" $
      E.number 1.23 @?= JsonNumber 1.23
  , testCase "null" $
      E.null @?= JsonNull
  , testCase "array" $
      E.array E.bool [True, False, True] @?= 
      JsonArray [JsonBool True, JsonBool False, JsonBool True]
  , testCase "object" $
      E.object E.bool [("true", True), ("false", False)] @?= 
      JsonObject [("true", JsonBool True), ("false", JsonBool False)]
  ]