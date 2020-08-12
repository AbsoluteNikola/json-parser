module TestJson (tests) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Parser (runParser)
import Json

tests :: TestTree
tests = testGroup "Json"
  [ testNull
  , testBool
  , testNumber
  , testString
  , testArray
  , testObject
  , testOther
  ]

testNull :: TestTree
testNull = testGroup "Null"
  [ testCase "ok" $
      runParser jsonNullP "null" @?= Just ("", JsonNull)
  , testCase "fail" $
      runParser jsonNullP "nulable" @?= Nothing
  ]

testBool :: TestTree
testBool = testGroup "Bool"
   [ testCase "true" $
       runParser jsonBoolP "true" @?= Just ("", JsonBool True)
   ,  testCase "false" $
       runParser jsonBoolP "false" @?= Just ("", JsonBool False)
   ,  testCase "fail" $
       runParser jsonBoolP "Fail" @?= Nothing
   ]

testString :: TestTree
testString = testGroup "String"
  [ testCase "\"true\"" $
      runParser jsonStringP "\"true\"" @?= Just ("", "true")
  , testCase "empty" $
      runParser jsonStringP "\"\"" @?= Just ("", "")
  , testCase "1" $
      runParser jsonStringP "\"1\"" @?= Just ("", "1")
  ]

testNumber :: TestTree
testNumber = testGroup "Number"
  [ testCase "123" $
      runParser jsonNumberP "123" @?= Just("", JsonNumber 123)
  ,  testCase "-123" $
      runParser jsonNumberP "-123" @?= Just("", JsonNumber (-123))
  ,  testCase "-123.0" $
      runParser jsonNumberP "-123.0" @?= Just("", JsonNumber (-123))
  ,  testCase "+123" $
      runParser jsonNumberP "+123" @?= Nothing
  ,  testCase "123.10" $
      runParser jsonNumberP "-123.10" @?= Just("", JsonNumber (-123.10))
  ,  testCase "-123." $
      runParser jsonNumberP "-123." @?= Just(".", JsonNumber (-123.0))
  ,  testCase "-123.1e1" $
      runParser jsonNumberP "-123.1e1" @?= Just("", JsonNumber (-1231))
  , testCase "5E+1" $
      runParser jsonNumberP "5E+1" @?= Just("", JsonNumber 50)
  , testCase "-5e-1" $
      runParser jsonNumberP "-5e-1" @?= Just("", JsonNumber (-0.5))
  , testCase "-5.5e-1" $
      runParser jsonNumberP "-5.5e-1" @?= Just("", JsonNumber (-0.55))
  ]

testArray :: TestTree
testArray = testGroup "Array"
  [ testCase "[1, 2, 3.9]" $
      runParser jsonArrayP "[1, 2, 3.9]" @?= Just("", JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3.9])
  , testCase "[1]" $
      runParser jsonArrayP "[1]" @?= Just("", JsonArray [JsonNumber 1])
  , testCase "[]" $
      runParser jsonArrayP "[]" @?= Just("", JsonArray [])
  , testCase "[\"hey\"  ,  123]" $
      runParser jsonArrayP "[\"hey\"  ,  123]" @?= Just("", JsonArray [JsonString "hey", JsonNumber 123])
  ]

testObject :: TestTree
testObject = testGroup "Object"
  [ testCase "{1: 1, 2: 2}" $
      runParser jsonObjectP "{\"1\":1,\"2\":2}" @?= Just("", JsonObject [("1", JsonNumber 1), ("2", JsonNumber 2)])
  , testCase "{1: 1}" $
      runParser jsonObjectP "{\"1\":1  }" @?= Just("", JsonObject [("1", JsonNumber 1)])
  , testCase "{}" $
      runParser jsonObjectP "{}" @?= Just("", JsonObject [])
  , testCase "{\"1\":[1, 2, 3, 4]}" $
      runParser jsonObjectP "{\"1\":[1, 2, 3, 4]}" @?=
        Just("", JsonObject [
          ("1", JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3, JsonNumber 4])])
  ]

testOther :: TestTree
testOther = testGroup "Other"
  [ testCase "whitespace" $
      runParser wsP " \t \nx" @?= Just ("x", " \t \n")
  ]

