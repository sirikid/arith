module Main where

import SpecUtils (describe, hspec)
import qualified Arith.LexerSpec as L
import qualified Arith.TokenSpec as T

main :: IO ()
main = hspec $ do
  describe "Arith" $ do
    describe "Token" T.spec
    describe "Lexer" L.spec
