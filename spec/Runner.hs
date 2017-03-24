module Main where

import SpecUtils (describe, hspec)
import qualified Arith.LexerSpec as L

main :: IO ()
main = hspec $ do
  describe "Arith" $ do
    describe "Lexer" L.spec
