{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Arith.Interpreter
  ( bigStep
  , evaluate
  , isNumericValue
  , isValue
  , normalize
  , smallStep
  ) where

import Arith.Term (Term(..))
import Control.Monad.Except (MonadError, throwError)

isNumericValue :: Term -> Bool
isNumericValue = \case
  TmZero -> True
  TmSucc t -> isNumericValue t
  _ -> False

isValue :: Term -> Bool
isValue = \case
  TmTrue -> True
  TmFalse -> True
  t -> isNumericValue t

smallStep :: MonadError String ex => Term -> ex Term
smallStep = go
  where
    -- E-IfTrue
    go (TmIf TmTrue t _) = pure t
    -- E-IfFalse
    go (TmIf TmFalse _ t) = pure t
    -- E-If
    go (TmIf t1 t2 t3) = TmIf <$> go t1 <*> pure t2 <*> pure t3
    -- E-Succ
    go (TmSucc t) = TmSucc <$> go t
    -- E-PredZero
    go (TmPred TmZero) = pure TmZero
    -- E-PredSucc
    go (TmPred (TmSucc t)) | isNumericValue t = pure t
    -- E-Pred
    go (TmPred t) = TmPred <$> go t
    -- E-IsZeroZero
    go (TmIsZero TmZero) = pure TmTrue
    -- E-IsZeroSucc
    go (TmIsZero (TmSucc t)) | isNumericValue t = pure TmFalse
    -- E-IsZero
    go (TmIsZero t) = TmIsZero <$> go t
    -- No rule applies
    go t = throwError ("Unevaluable term: " ++ show t)

normalize :: Term -> Term
normalize t = either (const t) normalize (smallStep t)

-- FIXME: Uninformative error messages
evaluate :: MonadError String ex => Term -> ex Term
evaluate t
  | isValue t = pure t
  | otherwise = smallStep t >>= evaluate

-- TODO: Implement
bigStep :: MonadError String ex => Term -> ex Term
bigStep = undefined
