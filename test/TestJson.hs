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
  [
  ]

testArray :: TestTree
testArray = testGroup "Array"
  [
  ]

testObject :: TestTree
testObject = testGroup "Object"
  [
  ]

