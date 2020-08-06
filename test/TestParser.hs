module TestParser where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Parser (runParser)
import Primitives (charP)

-- test this law
{-# ANN tests "HLint: ignore Functor law" #-}

tests :: TestTree
tests = testGroup "Test Parser" 
  [ testCase "id has no effect (1 functor law)" $
      runParser (id <$> charP 'x') "xy" @?= Just ("y", 'x') 
  ] 