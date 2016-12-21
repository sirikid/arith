module Main where

import Arith
import Data.Bifunctor (first)
import Data.List (isPrefixOf)
import System.IO

main :: IO ()
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
        putStrLn $ either id show $ return line >>= tokenize' >>= parse' >>= eval'
        repl
    tokenize' = first (("Unexpected character sequence: "++) . show) . tokenize
    parse'    = first (("Unexpected token sequence: "++)     . show) . parse
    eval'     = first (("Unevaluable term: "++)              . show) . eval
