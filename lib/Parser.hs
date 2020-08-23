module Parser (
-- * Parser
  Parser
, runParser
-- * Combinators
, getP
, takeAnyP
, takeWhileP
, charP
, charIfP
, stringP
, sepBy
, sepBy1
) where

import Parser.Base
import Parser.Combinators
