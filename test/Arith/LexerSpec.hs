module Arith.LexerSpec
  ( spec
  ) where

import Arith.Lexer
import Control.Monad.Except (throwError)
import Test.Hspec (context, describe, it, shouldBe, Spec)

-- FIXME
actual `shouldReturn` expected = actual `shouldBe` return expected
actual `shouldThrow` expectedError = actual `shouldBe` throwError expectedError

spec :: Spec
spec = do
  describe "tokenize" $ do
    it "splits a string into a list of tokens" $ do
      let string = "if then else zero succ pred true false is_zero"
      let tokens = [KwIf,KwThen,KwElse,KwZero,KwSucc,KwPred,KwTrue,KwFalse,KwIsZero]
      tokenize string `shouldReturn` tokens

    context "when an empty string given" $ do
      it "returns an empty list" $ do
        tokenize "" `shouldReturn` []

    context "when a given string contains only whitespace characters" $ do
      it "returns an empty list" $ do
        tokenize " " `shouldReturn` []
        tokenize "\n\r\t\v" `shouldReturn` []

    context "when a single keyword given" $ do
      it "returns a single token" $ do
        tokenize "if" `shouldReturn` [KwIf]
        tokenize "then" `shouldReturn` [KwThen]
        tokenize "else" `shouldReturn` [KwElse]
        tokenize "zero" `shouldReturn` [KwZero]
        tokenize "succ" `shouldReturn` [KwSucc]
        tokenize "pred" `shouldReturn` [KwPred]
        tokenize "true" `shouldReturn` [KwTrue]
        tokenize "false" `shouldReturn` [KwFalse]
        tokenize "is_zero" `shouldReturn` [KwIsZero]

    context "when a sequence of keywords given" $ do
      it "returns a sequence of tokens" $ do
        tokenize "zero" `shouldReturn` [KwZero]
        tokenize "zero true" `shouldReturn` [KwZero,KwTrue]
        tokenize "zero true false" `shouldReturn` [KwZero,KwTrue,KwFalse]

    context "when given string contains non-keyword" $ do
      it "fails with most left non-keyword" $ do
        tokenize "iff than els" `shouldThrow` "Unexpected character sequence: \"iff\""
        tokenize "if  than els" `shouldThrow` "Unexpected character sequence: \"than\""
        tokenize "if  then els" `shouldThrow` "Unexpected character sequence: \"els\""
