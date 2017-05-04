{-# LANGUAGE FlexibleContexts #-}

module Arith.Lexer
  ( Token(..)
  , tokenizeExpression
  , tokenize
  ) where

import Arith.Token (Token(..), fromString)
import Control.Monad.Except (MonadError, throwError)
import Data.List (groupBy)

-- | This function is a special version of tokenize, which is designed to work
-- with separated expressions. Appends a EndOfExpression token to the result.
tokenizeExpression :: MonadError String ex => String -> ex [Token]
tokenizeExpression = fmap (++ [EndOfExpression]) . tokenize

tokenize :: MonadError String ex => String -> ex [Token]
tokenize = traverse fromString . concatMap (groupBy parens) . words
  where
    parens a b =
      a /= '(' && a /= ')' &&
      b /= '(' && b /= ')'
