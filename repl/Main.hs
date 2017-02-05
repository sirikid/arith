module Main where

import Arith
import Data.List (isPrefixOf)
import System.Console.Readline (addHistory, readline)

main :: IO ()
main = do
  putStrLn "Type :q for quit"
  go
  where
    go = do
      mbLine <- readline " >> "
      case mbLine of
        Nothing -> pure ()
        Just cmd | ":q" `isPrefixOf` cmd -> pure ()
        Just expr -> addHistory expr >> putStrLn (either id show $ compute $ expr ++ " ;") >> go
    compute expr = tokenize expr >>= parse >>= evaluate
