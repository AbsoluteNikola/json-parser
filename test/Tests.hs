import Test.Tasty (TestTree, testGroup, defaultMain)
import qualified TestPrimitives
import qualified TestParser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
  [ TestParser.tests
  , TestPrimitives.tests 
  ]
  
