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
      let tokens = [KwTrue, KwFalse, KwIf, KwThen, KwElse, KwZero, KwSucc, KwPred, KwIsZero, LeftParen, RightParen, EndOfExpression]
      tokenize string `shouldReturn` tokens

    context "when given an empty string" $ do
      it "returns an empty sequence" $ do
        tokenize "" `shouldReturn` []

    -- TODO Rest cases
