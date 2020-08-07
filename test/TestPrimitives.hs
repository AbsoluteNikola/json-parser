module TestPrimitives (tests) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Parser (runParser)
import Primitives

tests :: TestTree
tests = testGroup "Test parser primitives" 
  [ testCase "charP" $ 
      runParser (charP 'x') "xy" @?= Just ("y", 'x')
  , testCase "stringP" $
      runParser (stringP "xy") "xyz" @?= Just ("z", "xy")
  ]