{-# LANGUAGE FlexibleContexts #-}

module Arith.Lexer
  ( Token(..)
  , tokenize
  ) where

import Data.Maybe (maybe)
import Control.Monad.Except (throwError)

data Token = KwIf | KwThen | KwElse | KwZero | KwSucc | KwPred | KwTrue | KwFalse | KwIsZero
  deriving (Eq, Show)

tokenize :: String -> Either String [Token]
tokenize input = fmap reverse $ foldl prependToken (return []) $ words input
  where
    prependToken acc word = acc >>= \ts -> fmap (:ts) $ intoToken word
    intoToken word = maybe (throwError $ "Unexpected character sequence: " ++ show word) return $ lookup word wordsToTokens
    wordsToTokens =
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
