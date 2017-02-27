{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Arith.Lexer
  ( Token(..)
  , tokenize
  ) where

import Control.Monad.Except (MonadError, throwError)
import Data.Char (isDigit)
import Data.List (groupBy)

data Token = KwIf | KwThen | KwElse | KwZero | KwSucc | KwPred | KwTrue
  | KwFalse | KwIsZero | EndOfExpression | Literal Integer
  deriving (Eq, Show)

tokenize :: MonadError String r => String -> r [Token]
tokenize = traverse intoToken . concatMap (groupBy separators) . words
  where
    -- TODO Multicharacter separators and operators
    separators a b = not (a == ';' || b == ';')

intoToken :: MonadError String r => String -> r Token
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
  wtf -> if isValidLiteral wtf
    then pure $ Literal (read wtf :: Integer)
    else throwError $ "Unexpected character sequence: " ++ show wtf
  where
    isValidLiteral = \case
      '-':chars@(_:_) -> isDigit `all` chars
      chars@(_:_) -> isDigit `all` chars
      _ -> False
