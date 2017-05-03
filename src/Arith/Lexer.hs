{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Arith.Lexer
  ( Token(..)
  , TokenizationFailure(..)
  , tokenizeExpression
  , tokenize
  ) where

import Control.Monad.Except (MonadError, throwError)
import Data.List (groupBy)

data Token
  -- Common tokens, corresponding to the constructions of the described
  -- language.
  = KwTrue
  | KwFalse
  | KwIf | KwThen | KwElse
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

data TokenizationFailure
  = UnexpectedSequence String
  deriving (Eq, Show)

-- | This function is a special version of tokenize, which is designed to work
-- with separated expressions. Appends a EndOfExpression token to the result.
tokenizeExpression :: MonadError TokenizationFailure ex => String -> ex [Token]
tokenizeExpression = fmap (++ [EndOfExpression]) . tokenize

tokenize :: MonadError TokenizationFailure ex => String -> ex [Token]
tokenize = traverse intoToken . concatMap (groupBy parens) . words
  where
    parens a b =
      a /= '(' && a /= ')' &&
      b /= '(' && b /= ')'

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
  wtf -> throwError (UnexpectedSequence wtf)
