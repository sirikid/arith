{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Arith.Token
  ( Token(..)
  , fromString
  ) where

import Control.Monad.Except (MonadError, throwError)

data Token
  -- Common tokens, corresponding to the constructions of the described
  -- language.
  = KwTrue
  | KwFalse
  | KwIf
  | KwThen
  | KwElse
  | KwZero
  | KwSucc
  | KwPred
  | KwIsZero
  | LeftParen
  | RightParen
  -- NOTE: This is a synthetic token which must identify the end of expression.
  -- This token should not be the result of lexical parsing.
  | EndOfExpression
  deriving (Eq, Show)

fromString :: MonadError String ex => String -> ex Token
fromString = \case
  "true" -> pure KwTrue
  "false" -> pure KwFalse
  "if" -> pure KwIf
  "then" -> pure KwThen
  "else" -> pure KwElse
  "0" -> pure KwZero
  "succ" -> pure KwSucc
  "pred" -> pure KwPred
  "iszero" -> pure KwIsZero
  "(" -> pure LeftParen
  ")" -> pure RightParen
  "" -> throwError "Empty string"
  wtf -> throwError ("Unexpected character sequence: " ++ show wtf)
