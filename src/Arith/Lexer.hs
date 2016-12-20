module Arith.Lexer
  ( Token(..)
  , tokenize
  ) where

import Data.Maybe (maybe)

data Token = KwTrue | KwFalse | KwIf | KwThen | KwElse | KwZero | KwSucc | KwPred | KwIsZero
  deriving (Eq, Show)

tokenize :: String -> Either String [Token]
tokenize input = fmap reverse $ foldl tryParseKw (Right []) $ words input where
  tryParseKw acc str = acc >>= \ts -> fmap (:ts) $ lookup' str
  lookup' str = maybe (Left str) Right $ lookup str kwMap
  kwMap =
    [("true"    ,KwTrue  )
    ,("false"   ,KwFalse )
    ,("if"      ,KwIf    )
    ,("then"    ,KwThen  )
    ,("else"    ,KwElse  )
    ,("0"       ,KwZero  )
    ,("succ"    ,KwSucc  )
    ,("pred"    ,KwPred  )
    ,("is_zero" ,KwIsZero)
    ]
