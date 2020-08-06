module TestPrimitives where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Parser (runParser)
import Primitives

tests :: TestTree
tests = testGroup "Test parser primitives" 
  [ testCase "runParser (charP 'x') \"xy\" == Just (\"y\", 'x')" $ 
       runParser (charP 'x') "xy" @?= Just ("y", 'x')
  ]