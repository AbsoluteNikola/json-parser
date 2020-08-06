module Parser (Parser(..)) where

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
--fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser helper
    where
      helper input = 
        case p input of
          Nothing      -> Nothing
          Just (xs, x) -> Just (xs, f x)
