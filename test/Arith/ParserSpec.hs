module Arith.ParserSpec
  ( spec
  ) where

import Arith.Lexer (Token(..))
import Arith.Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "parse" $ do
    it "builds abstract syntactic tree from the list of tokens" $ do
      True `shouldBe` True

    context "when an empty list given" $ do
      it "fails" $ do
        parse [] `shouldBe` Left []

    context "when a constant given" $ do
      it "returns corresponding term" $ do
        parse [KwZero] `shouldBe` Right TmZero
        parse [KwTrue] `shouldBe` Right TmTrue
        parse [KwFalse] `shouldBe` Right TmFalse
