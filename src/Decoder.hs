module Decoder where

import Control.Applicative (Alternative, (<|>), empty)

import Json (Json(..))

newtype Decoder a = Decoder { runDecoder :: Json -> Maybe a }

instance Functor Decoder where
  fmap f d = Decoder func
    where
        func json = f <$> runDecoder d json

instance Applicative Decoder where
  pure x = Decoder $ \_ -> Just x
  af <*> ad = Decoder func
    where
      func json = do
        f <- runDecoder af json
        d <- runDecoder ad json
        pure $ f d

instance Alternative Decoder where
  empty = Decoder $ const Nothing
  d1 <|> d2 = Decoder func
    where
      func json = runDecoder d1 json <|> runDecoder d2 json


instance Monad Decoder where
  (>>=) m k = Decoder $ \json -> do
    x <- runDecoder m json
    runDecoder (k x) json

instance MonadFail Decoder where
  fail _ = Decoder $ const Nothing


null :: Decoder ()
null = Decoder func
  where
    func JsonNull = Just ()
    func _ = fail ""

bool :: Decoder Bool
bool = Decoder func
  where
    func (JsonBool b) = Just b
    func _ = fail ""

string :: Decoder String
string = Decoder func
  where
    func (JsonString s) = Just s
    func _ = fail ""

double :: Decoder Double
double = Decoder func
  where
    func (JsonNumber x) = Just x
    func _ = fail ""

list :: Decoder a -> Decoder [a]
list d = Decoder $ \json -> do
     arr <- runDecoder (Decoder func) json
     traverse (runDecoder d) arr
  where
    func (JsonArray arr) = Just arr
    func _ = fail ""

field :: String -> Decoder a -> Decoder a
field key d = Decoder $ \json -> do
  inner <- runDecoder (Decoder func) json
  runDecoder d inner
  where
    func (JsonObject pairs) = lookup key pairs
    func _ = fail ""
