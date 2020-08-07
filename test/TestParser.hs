module TestParser where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Parser (runParser)
import Primitives (charP)

tests :: TestTree
tests = testGroup "Test Parser" 
  [ testFunctor
  , testApplicative
  ]

-- test this law
{-# ANN testFunctor "HLint: ignore Functor law" #-}

testFunctor :: TestTree
testFunctor = testGroup "Test Functor"
  [ testCase "id has no effect (1 functor law)" $
      runParser (id <$> charP 'x') "xy" @?= Just ("y", 'x')
  ]
  
testApplicative :: TestTree
testApplicative = testGroup "Test Applicative"
  [ testCase "pair from two parsers" $
      runParser ((,) <$> charP 'x' <*> charP 'y') "xy" @?= Just ("", ('x', 'y'))
  ]