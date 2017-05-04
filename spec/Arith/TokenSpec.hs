module Arith.TokenSpec
  ( spec
  ) where

import Arith.Token
import SpecUtils

spec :: Spec
spec = do
  describe "fromString" $ do
    it "tries to convert the given string into a token" $ do
      fromString "true" `shouldReturn` KwTrue
      fromString "false" `shouldReturn` KwFalse
      fromString "if" `shouldReturn` KwIf
      fromString "then" `shouldReturn` KwThen
      fromString "else" `shouldReturn` KwElse
      fromString "0" `shouldReturn` KwZero
      fromString "succ" `shouldReturn` KwSucc
      fromString "pred" `shouldReturn` KwPred
      fromString "iszero" `shouldReturn` KwIsZero
      fromString "(" `shouldReturn` LeftParen
      fromString ")" `shouldReturn` RightParen

    context "when the given string is empty" $ do
      it "throws an error with an obvious message" $ do
        fromString "" `shouldThrowError` "Empty string"

    context "when the given string is not a valid token" $ do
      it "throws an error which shows the cause of the failure" $ do
        fromString "tru" `shouldThrowError` "Unexpected character sequence: \"tru\""
        fromString "1337" `shouldThrowError` "Unexpected character sequence: \"1337\""
        fromString "iff" `shouldThrowError` "Unexpected character sequence: \"iff\""
        fromString " " `shouldThrowError` "Unexpected character sequence: \" \""
        fromString "\0\a\b\f\n\r\t\v\"\'\\" `shouldThrowError`
          "Unexpected character sequence: \"\\NUL\\a\\b\\f\\n\\r\\t\\v\\\"\'\\\\\""
