{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module TestQQ where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Json (Json(..))
import QQ (json)

tests :: TestTree
tests = testGroup "QQ"
  [ testCase "string" $
      [json|"hello"|] @?= JsonString "hello"
  , testCase "complex" $
      [json|
      {
        "a": 1,
        "b" : {
          "c": null
        }
      }
      |] @?= JsonObject 
      [ ("a", JsonNumber 1)
      , ("b", JsonObject
          [ ("c", JsonNull) ]
        )
      ]
  ]