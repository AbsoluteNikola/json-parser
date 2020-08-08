module TestPrimitives (tests) where

import Data.Char (isDigit)

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Parser (runParser)
import Primitives

tests :: TestTree
tests = testGroup "Primitives"
  [ testCase "charP" $ 
      runParser (charP 'x') "xy" @?= Just ("y", 'x')
  , testCase "stringP" $
      runParser (stringP "xy") "xyz" @?= Just ("z", "xy")
  , testCase "parseIf 1" $
      runParser (parseIf isDigit) "123" @?= Just ("23", '1')
  , testCase "parseIf 2" $
      runParser (parseIf isDigit) "a123" @?= Nothing
  , testCase "spanP (empty)" $
      runParser (spanP isDigit) "a123" @?= Just ("a123", "")
  , testCase "spanP (ok)" $
      runParser (spanP isDigit) "123" @?= Just ("", "123")
  ]