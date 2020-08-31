module TestPrimitives (tests) where

import Data.Char (isDigit)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Parser

tests :: TestTree
tests = testGroup "Primitives"
  [ testCase "charP" $ 
      runParser (charP 'x') "xy" @?= Just ("y", 'x')
  , testCase "stringP" $
      runParser (stringP "xy") "xyz" @?= Just ("z", "xy")
  , testCase "charIfP 1" $
      runParser (charIfP isDigit) "123" @?= Just ("23", '1')
  , testCase "charIfP 2" $
      runParser (charIfP isDigit) "a123" @?= Nothing
  , testCase "takeWhileP (empty)" $
      runParser (takeWhileP isDigit) "a123" @?= Just ("a123", "")
  , testCase "takeWhileP (ok)" $
      runParser (takeWhileP isDigit) "123" @?= Just ("", "123")
  ]