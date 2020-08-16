module TestDecoder where

import qualified Data.Text as T

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Decoder
import Json (Json(..))

data Author = Author { name' :: T.Text, surname :: T.Text } deriving (Eq, Show)

-- support for Integers will be added in nearest future
data Book = Book
  { author :: Author
  , name :: T.Text
  , year :: Double
  , chapters :: [T.Text]
  } deriving (Eq, Show)


-- in memory of Alekseev Alexandr (he's still alive, don't worry)
bookJson :: Json
bookJson = JsonObject
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

book :: Book
book = Book
  { author = Author "Alexandr" "Alekseev"
  , name = "как выжить в долгопрудном. Честный гайд"
  , year = 2019
  , chapters =
    [ "Вступление"
    , "1. Принятие"
    , "2. Прибытиые"
    , "3. Принятие"
    , "4. Как победить тараканов в общаге"
    , "5. Считаем степуху в шавермах"
    , "6. Путешествие до набережной и обратно"
    , "7. Как правильно писать псж"
    ]
  }

testOther :: TestTree
testOther = testGroup "Complex"
  [ testCase "author" $
      runDecoder (field "author" authorD) bookJson @?= Just (author book)
  , testCase "book name" $
      runDecoder (field "name" string) bookJson @?= Just (name book)
  , testCase "author surname" $
      runDecoder (field "author" $ field "surname" string) bookJson @?= Just (surname $ author book)
  , testCase "chapters" $
      runDecoder (field "chapters" $ list string) bookJson @?= Just (chapters book)
  ]
    where
      authorD = Author <$> field "name" string <*> field "surname" string


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
  [ testCase "null" $
      runDecoder Decoder.null JsonNull @?= Just ()
  , testCase "null (fail)" $
      runDecoder Decoder.null (JsonBool True) @?= Nothing
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
  [ testCase "\"123\"" $
      runDecoder string (JsonString "123") @?= Just "123"
  , testCase "\"123\"" $
      runDecoder string JsonNull @?= Nothing
  ]

testNumber :: TestTree
testNumber = testGroup "Number"
  [ testCase "2.1" $
      runDecoder double (JsonNumber 2.1) @?= Just 2.1
  , testCase "2.1" $
      runDecoder double JsonNull @?= Nothing
  ]

testArray :: TestTree
testArray = testGroup "Array"
  [ testCase "[1, 2]" $
      runDecoder (list double) (JsonArray [JsonNumber 1, JsonNumber 2]) @?= Just [1, 2]
  , testCase "[1, \"2\"]" $
      runDecoder (list double) (JsonArray [JsonNumber 1, JsonString "2"]) @?= Nothing
  ]

testObject :: TestTree
testObject = testGroup "Object"
  [ testCase "{\"1\": true}" $
      runDecoder (field "1" bool) (JsonObject [("1", JsonBool True)]) @?= Just True
  , testCase "{\"1\": true} (fail)" $
      runDecoder (field "2" bool) (JsonObject [("1", JsonBool True)]) @?= Nothing
  ]
