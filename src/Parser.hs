module Parser (Parser(..)) where

import Control.Applicative (Alternative(..))

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

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