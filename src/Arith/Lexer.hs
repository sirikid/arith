{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Arith.Lexer
  ( Token(..)
  , TokenizationFailure(..)
  , tokenize
  ) where

import Control.Monad.Except (MonadError, throwError)
import Data.List (groupBy)

data Token
  = KwTrue
  | KwFalse
  | KwIf | KwThen | KwElse
  | KwZero
  | KwSucc
  | KwPred
  | KwIsZero
  | LeftParen
  | RightParen
  | Semicolon -- TODO Remove this
  deriving (Eq, Show)

data TokenizationFailure
  = UnexpectedSequence String
  deriving (Eq, Show)

tokenize :: MonadError TokenizationFailure ex => String -> ex [Token]
tokenize = traverse intoToken . concatMap (groupBy separators) . words
  where
    -- TODO Multicharacter separators and operators
    separators a b = not (a == ';' || b == ';')

intoToken :: MonadError TokenizationFailure ex => String -> ex Token
intoToken = \case
  "true" -> pure KwTrue
  "false" -> pure KwFalse
  "if" -> pure KwIf
  "then" -> pure KwThen
  "else" -> pure KwElse
  "zero" -> pure KwZero
  "succ" -> pure KwSucc
  "pred" -> pure KwPred
  "iszero" -> pure KwIsZero
  "(" -> pure LeftParen
  ")" -> pure RightParen
  ";" -> pure Semicolon
  wtf -> throwError . UnexpectedSequence $ wtf
