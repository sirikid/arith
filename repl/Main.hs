{-# LANGUAGE LambdaCase #-}

module Main where

import Arith (evaluateAndShow)
import Data.List (isPrefixOf)
import System.Console.Readline (addHistory, readline)

main :: IO ()
main = do
  putStrLn "Type :q for quit"
  go
  where
    go = readline " >> " >>= \case
      Nothing -> pure ()
      Just command | ":q" `isPrefixOf` command -> pure ()
      Just expression -> do
        addHistory expression
        putStrLn $ evaluateAndShow expression
        go
