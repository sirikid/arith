module Arith.Lexer
  ( Token(..)
  , tokenize
  ) where

data Token = KwTrue | KwFalse | KwIf | KwThen | KwElse | KwZero | KwSucc | KwPred | KwIsZero
  deriving (Eq, Show)

tokenize :: String -> Either String [Token]
tokenize = Left
