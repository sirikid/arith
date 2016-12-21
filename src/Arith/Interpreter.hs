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
isBooleanValue term = case term of
  TmTrue  -> True
  TmFalse -> True
  _       -> False

isNumericValue :: Term -> Bool
isNumericValue term = case term of
  TmZero -> True
  TmSucc t -> isNumericValue t
  TmPred t -> isNumericValue t
  _ -> False

eval :: Term -> Either Term Term
eval = Left
