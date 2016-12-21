module Arith.Interpreter
  ( eval
  , isValue
  , isBooleanValue
  , isNumericValue
  ) where

import Arith.Parser (Term(..))

isValue :: Term -> Bool
isValue term = isBooleanValue term || isNumericValue term

isBooleanValue :: Term -> Bool
isBooleanValue _ = False

isNumericValue :: Term -> Bool
isNumericValue _ = False

eval :: Term -> Either Term Term
eval = Left
