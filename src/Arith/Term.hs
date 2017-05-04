{-# LANGUAGE LambdaCase #-}

module Arith.Term
  ( Term(..)
  , constants
  , depth
  , size
  ) where

import Data.Set (Set, singleton, union)

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Ord)

instance Show Term where
  show = \case
    TmTrue -> "true"
    TmFalse -> "false"
    TmIf t1 t2 t3 ->
      "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3
    TmZero -> "0"
    TmSucc t -> "succ " ++ show t
    TmPred t -> "pred " ++ show t
    TmIsZero t -> "iszero " ++ show t

constants :: Term -> Set Term
constants = \case
  TmTrue -> singleton TmTrue
  TmFalse -> singleton TmFalse
  TmIf t1 t2 t3 -> constants t1 `union` constants t2 `union` constants t3
  TmZero -> singleton TmZero
  TmSucc t -> constants t
  TmPred t -> constants t
  TmIsZero t -> constants t

size :: Term -> Integer
size = \case
  TmTrue -> 1
  TmFalse -> 1
  TmIf t1 t2 t3 -> size t1 + size t2 + size t3 + 1
  TmZero -> 1
  TmSucc t -> size t + 1
  TmPred t -> size t + 1
  TmIsZero t -> size t + 1

depth :: Term -> Integer
depth = \case
  TmTrue -> 1
  TmFalse -> 1
  TmIf t1 t2 t3 -> (depth t1 `max` depth t2 `max` depth t3) + 1
  TmZero -> 1
  TmSucc t -> depth t + 1
  TmPred t -> depth t + 1
  TmIsZero t -> depth t + 1
