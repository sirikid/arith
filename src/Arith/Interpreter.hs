module Arith.Interpreter
  ( eval
  ) where

import Arith.Parser (Term(..))

eval :: Term -> Either Term Term
eval = Left
