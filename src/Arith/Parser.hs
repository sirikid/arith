module Arith.Parser
  ( Term
  , parse
  ) where

import Arith.Lexer (Token(..))

data Term = Term

parse :: [Token] -> Either [Token] Term
parse _ = undefined
