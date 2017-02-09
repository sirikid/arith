{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Arith.Lexer
  ( Token(..)
  , tokenize
  ) where

import Control.Monad.Except (throwError)
import Data.List (groupBy)

data Token = KwIf | KwThen | KwElse | KwZero | KwSucc | KwPred | KwTrue | KwFalse | KwIsZero | EndOfExpression
  deriving (Eq, Show)

tokenize :: String -> Either String [Token]
tokenize
  = fmap reverse
  . foldl keepFirstError (pure [])
  . fmap intoToken
  . concatMap (groupBy separators)
  . words
  where
    -- TODO Multicharacter separators and operators
    separators a b = not $ separator `any` [[a], [b], [a, b]]
    separator = (== ";")
    keepFirstError error@(Left _) = const error
    keepFirstError (Right tokens) = \case
      Left error -> throwError error
      Right nextToken -> pure (nextToken:tokens)

intoToken :: String -> Either String Token
intoToken = \case
  ";" -> pure EndOfExpression
  "else" -> pure KwElse
  "false" -> pure KwFalse
  "if" -> pure KwIf
  "is_zero" -> pure KwIsZero
  "pred" -> pure KwPred
  "succ" -> pure KwSucc
  "then" -> pure KwThen
  "true" -> pure KwTrue
  "zero" -> pure KwZero
  wtf -> throwError $ "Unexpected character sequence: " ++ show wtf
