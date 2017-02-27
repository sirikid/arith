{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Arith.Parser
  ( Term(..)
  , parse
  ) where

import Arith.Lexer (Token(..))
import Control.Monad.Except (MonadError, throwError)
import Data.List (insertBy)

data Term = TmZero | TmTrue | TmFalse | TmSucc Term | TmPred Term
  | TmIsZero Term | TmIf Term Term Term
  deriving Eq

instance Show Term where
  show = \case
    TmZero -> "zero"
    TmTrue -> "true"
    TmFalse -> "false"
    (TmSucc t) -> "succ " ++ show t
    (TmPred t) -> "pred " ++ show t
    (TmIf c t e) -> "if " ++ show c ++ " then " ++ show t ++ " else " ++ show e

parse :: MonadError String r => [Token] -> r Term
parse tokens = do
  (ts, tm) <- lookForExpression $ concatMap expandLiteral tokens
  if null ts
    then pure tm
    else throwError $ "Unutilized tokens: " ++ show ts
  where
    expandLiteral = \case
      Literal lit -> insertBy (\_ _ -> GT) KwZero $ replicate (fromInteger $ abs lit) $ if lit > 0 then KwSucc else KwPred
      nonLiteral -> [nonLiteral]

lookUntil :: MonadError String r => (Token -> Bool) -> [Token] -> r ([Token], Term)
lookUntil predicate = \case
  [] -> throwError "Unexpected end of sequence"
  [EndOfExpression] -> throwError "Unexpected end of expression"
  (KwZero:t:ts) | predicate t -> pure (ts, TmZero)
  (KwTrue:t:ts) | predicate t -> pure (ts, TmTrue)
  (KwFalse:t:ts) | predicate t -> pure (ts, TmFalse)
  (KwSucc:ts) -> fmap TmSucc <$> go ts
  (KwPred:ts) -> fmap TmPred <$> go ts
  (KwIsZero:ts) -> fmap TmIsZero <$> go ts
  (KwIf:ts) -> do
    (ts, condition) <- lookForCondition ts
    (ts, thenBranch) <- lookForThenBranch ts
    (ts, elseBranch) <- lookForElseBranch ts
    pure (ts, TmIf condition thenBranch elseBranch)
  wtf -> throwError $ "Unparsable token sequence: " ++ show wtf
  where
    go = lookUntil predicate

lookForExpression, lookForCondition, lookForThenBranch, lookForElseBranch :: MonadError String r => [Token] -> r ([Token], Term)
lookForExpression = lookUntil (== EndOfExpression)
lookForCondition  = lookUntil (== KwThen)
lookForThenBranch = lookUntil (== KwElse)
lookForElseBranch = lookUntil (`elem` [KwThen, KwElse, EndOfExpression])
