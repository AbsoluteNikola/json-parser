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
  [
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
  , testCase "5e+1" $
      runParser jsonNumberP "5e+1" @?= Just("", JsonNumber 50)
  , testCase "-5e-1" $
      runParser jsonNumberP "-5e-1" @?= Just("", JsonNumber (-0.5))
  , testCase "-5.5e-1" $
      runParser jsonNumberP "-5.5e-1" @?= Just("", JsonNumber (-0.55))
  ]

testArray :: TestTree
testArray = testGroup "Array"
  [
  ]

testObject :: TestTree
testObject = testGroup "Object"
  [
  ]
  
testOther :: TestTree
testOther = testGroup "Other"
  [ testCase "whitespace" $
      runParser wsP " \t \nx" @?= Just ("x", " \t \n")
  ]

