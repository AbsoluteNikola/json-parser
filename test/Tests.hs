import Test.Tasty (TestTree, testGroup, defaultMain)
import qualified TestPrimitives
import qualified TestParser
import qualified TestJson
import qualified TestDecoder
import qualified TestEncoder

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
  [ TestParser.tests
  , TestPrimitives.tests
  , TestJson.tests
  , TestDecoder.tests
  , TestEncoder.tests
  ]
  
