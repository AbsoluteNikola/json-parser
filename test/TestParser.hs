module TestParser (tests) where

import Control.Applicative((<|>))

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Parser (runParser, charP)

tests :: TestTree
tests = testGroup "Parser" 
  [ testFunctor
  , testApplicative
  , testAlternative
  ]

-- test this law
{-# ANN testFunctor ("HLint: ignore Functor law" :: String) #-}

testFunctor :: TestTree
testFunctor = testGroup "Functor"
  [ testCase "id has no effect (1 functor law)" $
      runParser (id <$> charP 'x') "xy" @?= Just ("y", 'x')
  ]
  
testApplicative :: TestTree
testApplicative = testGroup "Applicative"
  [ testCase "pair from two parsers (ok)" $
      runParser ((,) <$> charP 'x' <*> charP 'y') "xy" @?= Just ("", ('x', 'y'))
  , testCase "pair from two parsers (fail)" $
      runParser ((,) <$> charP 'x' <*> charP 'y') "zy" @?= Nothing
  ]

testAlternative :: TestTree
testAlternative = testGroup "Alternative"
  [ testCase "'x' or 'y' from \"xy\"" $
      runParser (charP 'x' <|> charP 'y') "xy" @?= Just ("y", 'x')
  , testCase "'x' or 'y' from \"zx\"" $
      runParser (charP 'x' <|> charP 'y') "zx" @?= Nothing
  ]
