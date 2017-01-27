{-# LANGUAGE FlexibleContexts #-}

module Arith.Interpreter
  ( eval
  ) where

import Arith.Parser (Term(..))
import Control.Monad.Except (throwError)

isValue :: Term -> Bool
isValue term = isBooleanValue term || isNumericValue term

isBooleanValue :: Term -> Bool
isBooleanValue term = case term of
  TmTrue  -> True
  TmFalse -> True
  _       -> False

isNumericValue :: Term -> Bool
isNumericValue term = case term of
  TmZero   -> True
  TmSucc t -> isNumericValue t
  TmPred t -> isNumericValue t
  _        -> False

eval :: Term -> Either String Term
eval term = go term
  where
    go (TmIf cond ifTrue ifFalse) = case cond of
      TmTrue  -> Right $ ifTrue
      TmFalse -> Right $ ifFalse
      _       -> go cond >>= \cond' -> go $ TmIf cond' ifTrue ifFalse
    go (TmIsZero n) | isNumericValue n = isZero <$> go n
    go (TmSucc (TmPred t)) | isNumericValue t = go t
    go (TmPred (TmSucc t)) | isNumericValue t = go t
    go t = if isValue t
      then return t
      else throwError $ "Unevaluable term: " ++ show t
    isZero n | isNumericValue n = if n == TmZero
      then TmTrue
      else TmFalse
