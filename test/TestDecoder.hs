module TestDecoder where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Decoder
import Json (Json(..))

-- in memory of Alekseev Alexandr (he's still alive, don't worry)
sample :: Json
sample = JsonObject
  [ ("author", JsonObject 
      [ ("name", JsonString "Alexandr")
      , ("surname", JsonString "Alekseev")
      ]
    )
  , ("name", JsonString "как выжить в долгопрудном. Честный гайд")
  , ("year", JsonNumber 2019)
  , ("chapters", JsonArray 
      [ JsonString "Вступление"
      , JsonString "1. Принятие"
      , JsonString "2. Прибытиые"
      , JsonString "3. Принятие"
      , JsonString "4. Как победить тараканов в общаге"
      , JsonString "5. Считаем степуху в шавермах"
      , JsonString "6. Путешествие до набережной и обратно"
      , JsonString "7. Как правильно писать псж"
      ]
    )
  ]


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
  [ 
  ]

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
