{-# LANGUAGE FlexibleContexts #-}

module Arith.Parser
  ( Term(..)
  , parse
  ) where

import Arith.Lexer (Token(..))
import Control.Monad.Except (throwError)

data Term = TmZero | TmTrue | TmFalse | TmSucc Term | TmPred Term | TmIsZero Term | TmIf Term Term Term
  deriving Eq

instance Show Term where
  show = go
    where
      go TmZero = "zero"
      go TmTrue = "true"
      go TmFalse = "false"
      go (TmSucc tm) = "succ " ++ show tm
      go (TmPred tm) = "pred " ++ show tm
      go (TmIf c tb eb) = "if " ++ show c ++ " then " ++ show tb ++ " else " ++ show eb

parse :: [Token] -> Either String Term
parse tokens = do
  (ts, tm) <- lookForExpression tokens
  if null ts
    then pure tm
    else throwError $ "Unutilized tokens: " ++ show ts

lookUntil :: (Token -> Bool) -> [Token] -> Either String ([Token], Term)
lookUntil predicate = go
  where
    go [] = throwError "Unexpected end of sequence"
    go [EndOfExpression] = throwError "Unexpected end of expression"
    go (KwZero:t:ts) | predicate t = pure (ts, TmZero)
    go (KwTrue:t:ts) | predicate t = pure (ts, TmTrue)
    go (KwFalse:t:ts) | predicate t = pure (ts, TmFalse)
    go (KwSucc:ts) = fmap TmSucc <$> go ts
    go (KwPred:ts) = fmap TmPred <$> go ts
    go (KwIsZero:ts) = fmap TmIsZero <$> go ts
    go (KwIf:ts) = do
      (ts, condition) <- lookForCondition ts
      (ts, thenBranch) <- lookForThenBranch ts
      (ts, elseBranch) <- lookForElseBranch ts
      pure (ts, TmIf condition thenBranch elseBranch)
    go ts = throwError $ "Unparsable token sequence: " ++ show ts

lookForExpression, lookForCondition, lookForThenBranch, lookForElseBranch :: [Token] -> Either String ([Token], Term)
lookForExpression = lookUntil (== EndOfExpression)
lookForCondition  = lookUntil (== KwThen)
lookForThenBranch = lookUntil (== KwElse)
lookForElseBranch = lookUntil (`elem` [KwThen, KwElse, EndOfExpression])
