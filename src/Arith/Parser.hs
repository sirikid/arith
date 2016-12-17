module Arith.Parser
  ( Term(..)
  , parse
  ) where

import Arith.Lexer (Token(..))

data Term = Term
  deriving Show

parse :: [Token] -> Either [Token] Term
parse = Left
