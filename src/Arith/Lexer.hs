module Arith.Lexer
  ( Token
  , tokenize
  ) where

data Token = Token

tokenize :: String -> Either String [Token]
tokenize _ = undefined
