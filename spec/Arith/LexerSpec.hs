module Arith.LexerSpec
  ( spec
  ) where

import Arith.Lexer
import SpecUtils

spec :: Spec
spec = do
  describe "tokenize" $ do
    it "tries to read the sequence of tokens from the string" $ do
      let string = "true false if then else zero succ pred iszero ( ) ;"
      let tokens = [KwTrue, KwFalse, KwIf, KwThen, KwElse, KwZero, KwSucc, KwPred, KwIsZero, LeftParen, RightParen, Semicolon]
      tokenize string `shouldReturn` tokens

    context "when given an empty string" $ do
      it "returns an empty sequence" $ do
        tokenize "" `shouldReturn` []

    context "when given a string with single keyword or separator" $ do
      it "returns one-item list with corresponding token" $ do
        tokenize "true" `shouldReturn` [KwTrue]
        tokenize "false" `shouldReturn` [KwFalse]
        tokenize "if" `shouldReturn` [KwIf]
        tokenize "then" `shouldReturn` [KwThen]
        tokenize "else" `shouldReturn` [KwElse]
        tokenize "zero" `shouldReturn` [KwZero]
        tokenize "succ" `shouldReturn` [KwSucc]
        tokenize "pred" `shouldReturn` [KwPred]
        tokenize "iszero" `shouldReturn` [KwIsZero]
        tokenize "(" `shouldReturn` [LeftParen]
        tokenize ")" `shouldReturn` [RightParen]
        tokenize ";" `shouldReturn` [Semicolon]

    -- TODO Rest cases
