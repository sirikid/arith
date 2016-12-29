module Arith.Lexer
  ( Token(..)
  , tokenize
  ) where

import Data.Maybe (maybe)

data Token = KwIf | KwThen | KwElse | KwZero | KwSucc | KwPred | KwTrue | KwFalse | KwIsZero
  deriving (Eq, Show)

tokenize :: String -> Either String [Token]
tokenize input = fmap reverse $ foldl tryParseKw (Right []) $ words input where
  tryParseKw acc str = acc >>= \ts -> fmap (:ts) $ lookup' str
  lookup' str = maybe (Left str) Right $ lookup str kwMap
  kwMap =
    [("if"      ,KwIf    )
    ,("then"    ,KwThen  )
    ,("else"    ,KwElse  )
    ,("zero"    ,KwZero  )
    ,("succ"    ,KwSucc  )
    ,("pred"    ,KwPred  )
    ,("true"    ,KwTrue  )
    ,("false"   ,KwFalse )
    ,("is_zero" ,KwIsZero)
    ]
