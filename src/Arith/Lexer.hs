{-# LANGUAGE FlexibleContexts #-}

module Arith.Lexer
  ( Token(..)
  , tokenize
  ) where

import Control.Monad.Except (throwError)
import Data.List (groupBy)
import Data.Maybe (maybe)

data Token = KwIf | KwThen | KwElse | KwZero | KwSucc | KwPred | KwTrue | KwFalse | KwIsZero | EndOfExpression
  deriving (Eq, Show)

(<&>) = flip (<$>)

tokenize :: String -> Either String [Token]
tokenize = fmap reverse . foldl prependToken (pure []) . concatMap (groupBy semicolon) . words
  where
    semicolon a b = ';' `notElem` [a, b]
    prependToken acc word = acc >>= \ts -> intoToken word <&> (:ts)
    intoToken word = maybe (unexpected word) pure $ lookup word wordsToTokens
    unexpected = throwError . ("Unexpected character sequence: " ++) . show
    wordsToTokens =
      [ ("if"      , KwIf)
      , ("then"    , KwThen)
      , ("else"    , KwElse)
      , ("zero"    , KwZero)
      , ("succ"    , KwSucc)
      , ("pred"    , KwPred)
      , ("true"    , KwTrue)
      , ("false"   , KwFalse)
      , ("is_zero" , KwIsZero)
      , (";"       , EndOfExpression)
      ]
