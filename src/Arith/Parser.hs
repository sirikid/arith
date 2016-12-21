module Arith.Parser
  ( Term(..)
  , parse
  ) where

import Arith.Lexer (Token(..))

data Term = TmZero | TmTrue | TmFalse | TmSucc Term | TmPred Term | TmIsZero Term | TmIf Term Term Term
  deriving (Eq, Show)

parse :: [Token] -> Either [Token] Term
parse = Left
