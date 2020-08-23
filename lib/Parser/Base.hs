module Parser.Base (
  Parser
, runParser
, getP
, takeAnyP
, takeWhileP
) where

import Control.Applicative (Alternative(..))
import Data.Tuple (swap)
import qualified Data.Text as T


-- Type

-- | StateT T.Text Maybe
newtype Parser a = Parser { runParser :: T.Text -> Maybe (T.Text, a) }


-- Instances

instance Functor Parser where
--fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser helper
    where
      helper input =
        case p input of
          Nothing      -> Nothing
          Just (xs, x) -> Just (xs, f x)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s, x)
  pf <*> p = Parser func
    where
      func s = do
        (s', f) <- runParser pf s
        (s'', x) <- runParser p s'
        pure (s'', f x)

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser func
    where 
      func s = runParser p1 s <|> runParser p2 s
      
instance Monad Parser where
  return = pure
  m >>= k = Parser func
    where
      func s = do
        (s', x) <- runParser m s
        (s'', y) <- runParser (k x) s'
        pure (s'', y)
        
instance MonadFail Parser where
  fail _ = Parser $ const Nothing
  

-- Primitives

getP :: Parser T.Text
getP = Parser $ \s -> Just (s, s)

takeAnyP :: Parser Char
takeAnyP = Parser $ \s -> swap <$> T.uncons s

takeWhileP :: (Char -> Bool) -> Parser T.Text
takeWhileP p = Parser $ \s -> Just (T.dropWhile p s, T.takeWhile p s)
