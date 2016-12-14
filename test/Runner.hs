module Main where

import Test.Hspec
import qualified Arith.LexerSpec       as L
import qualified Arith.ParserSpec      as P
import qualified Arith.InterpreterSpec as I

main :: IO ()
main = hspec $ do
  describe "Arith" $ do
    describe "Lexer"       L.spec
    describe "Parser"      P.spec
    describe "Interpreter" I.spec
