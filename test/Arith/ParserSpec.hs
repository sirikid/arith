module Arith.ParserSpec
  ( spec
  ) where

import Arith.Lexer (Token(..))
import Arith.Parser
import SpecUtils (Spec, context, describe, it, shouldReturn, shouldThrow)

spec :: Spec
spec = do
  describe "parse" $ do
    it "builds abstract syntactic tree from the list of tokens" $ do
      let tokens = [KwIf, KwIsZero, KwSucc, KwPred, KwZero, KwThen, KwTrue, KwElse, KwSucc, KwFalse, EndOfExpression]
      let term = TmIf (TmIsZero (TmSucc (TmPred TmZero))) TmTrue (TmSucc TmFalse)
      parse tokens `shouldReturn` term

    context "when an empty list given" $ do
      it "fails with obscure error message" $ do
        parse [] `shouldThrow` "Unexpected end of sequence"

    context "when given a single EndOfExpression token" $ do
      it "fails with a bit more informative message" $ do
        parse [EndOfExpression] `shouldThrow` "Unexpected end of expression"

    context "when given a token, that represents a value, and then EndOfExpression" $ do
      it "returns corresponding value as a term" $ do
        parse [KwZero, EndOfExpression] `shouldReturn` TmZero
        parse [KwTrue, EndOfExpression] `shouldReturn` TmTrue
        parse [KwFalse, EndOfExpression] `shouldReturn` TmFalse

    context "when given a token, that represents an unary operator, and then any expression" $ do
      it "returns corresponding unary operator applied to that expression" $ do
        parse [KwSucc, KwZero, EndOfExpression] `shouldReturn` TmSucc TmZero
        parse [KwPred, KwPred, KwTrue, EndOfExpression] `shouldReturn` TmPred (TmPred TmTrue)
        parse [KwIsZero, KwSucc, KwIsZero, KwPred, KwFalse, EndOfExpression] `shouldReturn` TmIsZero (TmSucc (TmIsZero (TmPred TmFalse)))

    -- TODO Specify "if-then-else" parsing
