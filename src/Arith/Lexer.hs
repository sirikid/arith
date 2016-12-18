module Arith.Lexer
  ( Token(..)
  , tokenize
  ) where

import Data.Maybe (fromMaybe)

data Token = KwTrue | KwFalse | KwIf | KwThen | KwElse | KwZero | KwSucc | KwPred | KwIsZero
  deriving (Eq, Show)

tokenize :: String -> Either String [Token]
tokenize input = fmap reverse $ foldl tryParseKw (Right []) $ words input where
  tryParseKw acc word = case acc of
    Left string -> Left $ string ++ " " ++ word
    Right tokens -> fromMaybe (Left word) (fmap (Right . (:tokens)) (lookup word kwMap))
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
