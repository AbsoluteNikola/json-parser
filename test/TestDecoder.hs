module TestDecoder where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Decoder
import Json (Json(..))

tests :: TestTree
tests = testGroup "Decoder"
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
  []

testBool :: TestTree
testBool = testGroup "Bool"
   [ testCase "true" $
       runDecoder bool (JsonBool True) @?= Just True
   , testCase "false" $
       runDecoder bool (JsonBool False) @?= Just False
   , testCase "\"123\"" $
       runDecoder bool (JsonString "123") @?= Nothing
   ]

testString :: TestTree
testString = testGroup "String"
  []

testNumber :: TestTree
testNumber = testGroup "Number"
  []

testArray :: TestTree
testArray = testGroup "Array"
  []

testObject :: TestTree
testObject = testGroup "Object"
  [ testCase "{\"1\": true}" $
      runDecoder (field "1" bool) (JsonObject [("1", JsonBool True)]) @?= Just True
  ]

testOther :: TestTree
testOther = testGroup "Other"
  []
