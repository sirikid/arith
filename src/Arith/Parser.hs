{-# LANGUAGE FlexibleContexts #-}

module Arith.Parser
  ( Term(..)
  , parse
  ) where

import Arith.Lexer (Token(..))
import Control.Monad.Except (throwError)

data Term = TmZero | TmTrue | TmFalse | TmSucc Term | TmPred Term | TmIsZero Term | TmIf Term Term Term
  deriving (Eq, Show)

parse :: [Token] -> Either String Term
parse = throwError . ("Unparsable token sequence: "++) . show
