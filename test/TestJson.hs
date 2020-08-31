module TestJson (tests) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Text (pack)
import Json

tests :: TestTree
tests = testGroup "Json"
  [ testNull
  , testBool
  , testString
  , testNumber
  , testArray
  , testObject
  ]

testNull :: TestTree
testNull = testGroup "Null"
  [ testJson "null"    $ Just JsonNull
  , testJson "nulable" $ Nothing
  ]

testBool :: TestTree
testBool = testGroup "Bool"
   [ testJson "true" $ Just (JsonBool True)
   , testJson "false" $ Just (JsonBool False)
   , testJson "True" $ Nothing
   , testJson "False" $ Nothing
   ]

testString :: TestTree
testString = testGroup "String"
  [ testJson "\"true\"" $ Just (JsonString "true")
  , testJson "\"\"" $ Just (JsonString "")
  , testJson "\"1\"" $ Just (JsonString "1")
  ]

testNumber :: TestTree
testNumber = testGroup "Number"
  [ testJson "123" $ Just (JsonNumber 123)
  , testJson "-123" $ Just (JsonNumber (-123))
  , testJson "-123.0" $ Just (JsonNumber (-123))
  , testJson "+123" $ Nothing
  , testJson "-123.10" $ Just (JsonNumber (-123.10))
  , testJson "-123." $ Nothing
  , testJson "-123.1e1" $ Just (JsonNumber (-1231))
  , testJson "5E+1" $ Just (JsonNumber 50)
  , testJson "-5e-1" $ Just (JsonNumber (-0.5))
  , testJson "-5.5e-1" $ Just (JsonNumber (-0.55))
  ]

testArray :: TestTree
testArray = testGroup "Array"
  [ testJson "[1, 2, 3.9]" $ Just (JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3.9])
  , testJson "[1]" $ Just (JsonArray [JsonNumber 1])
  , testJson "[]" $ Just (JsonArray [])
  , testJson "[\"hey\"  ,  123]" $ Just (JsonArray [JsonString "hey", JsonNumber 123])
  ]

testObject :: TestTree
testObject = testGroup "Object"
  [ testJson "{\"1\":1,\"2\":2}" $ Just (JsonObject [("1", JsonNumber 1), ("2", JsonNumber 2)])
  , testJson "{\"1\":1  }" $ Just (JsonObject [("1", JsonNumber 1)])
  , testJson "{}" $ Just (JsonObject [])
  , testJson "{\"1\":[1, 2, 3, 4]}" $
        Just (JsonObject [
          ("1", JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3, JsonNumber 4])])
  ]


testJson :: String -> Maybe Json -> TestTree
testJson s mj =
  testCase (show s) $
    parseJson (pack s) @?= mj
