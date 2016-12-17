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
    tokenize' = first (("Can't tokenize string "++) . show) . tokenize
    parse'    = first (("Can't parse tokens "++) . show) . parse
    eval'     = first (("Can't evaluate AST "++) . show) . eval
