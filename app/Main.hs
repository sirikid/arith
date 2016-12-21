module Main where

import Arith
import Data.Bifunctor (first)
import Data.List (isPrefixOf)
import System.IO (hFlush,stdout)

main :: IO ()
main = do
  loop "Type :q for quit"
  where
    loop prev = do
      putStr $ prev ++ "\n >> "
      hFlush stdout
      line <- getLine
      if ":q" `isPrefixOf` line
        then return ()
        else loop $ evalString line
    evalString s = either id show $ do
      tokens <- tokenize' s
      ast    <- parse' tokens
      result <- eval' ast
      return result
    tokenize' = first (("Unexpected character sequence: "++) . show) . tokenize
    parse'    = first (("Unexpected token sequence: "++)     . show) . parse
    eval'     = first (("Unevaluable term: "++)              . show) . eval
