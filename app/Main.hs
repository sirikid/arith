module Main where

import Arith
import Data.Bifunctor
import Data.List
import System.IO

data Failure = TokenizationF String | ParsingF [Token] | EvaluationF Term

instance Show Failure where
  show failure = case failure of
    TokenizationF cause -> "can't tokenize " ++ show cause
    ParsingF      cause -> "can't parse " ++ show cause
    EvaluationF   cause -> "can't eval " ++ show cause

main = do
  putStrLn "Type :q for quit"
  repl where
    repl = do
      putStr " >> "
      hFlush stdout
      line <- getLine
      if ":q" `isPrefixOf` line
        then return ()
        else do
        putStrLn $ showEF $ return line >>= tokenize' >>= parse' >>= eval'
        repl
    showEF (Left failure) = "Failure: " ++ show failure
    showEF (Right result) = " => " ++ show result
    tokenize' = first TokenizationF . tokenize
    parse'    = first ParsingF . parse
    eval'     = first EvaluationF . eval
