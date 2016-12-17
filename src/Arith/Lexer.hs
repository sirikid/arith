module Arith.Lexer
  ( Token(..)
  , tokenize
  ) where

data Token = Token
  deriving Show

tokenize :: String -> Either String [Token]
tokenize = Left
