module Arith.InterpreterSpec
  ( spec
  ) where

import Test.Hspec
import Arith.Parser (Term(..))
import Arith.Interpreter

spec :: Spec
spec = do
  describe "isBooleanValue" $ do

    it "returns True if argument is TmTrue" $ do
      isBooleanValue TmTrue `shouldBe` True

    it "returns True if argument is TmFalse" $ do
      isBooleanValue TmFalse `shouldBe` True

    it "returns False otherwise" $ do
      isBooleanValue TmZero `shouldBe` False
      isBooleanValue (TmSucc TmZero) `shouldBe` False
      isBooleanValue (TmPred TmTrue) `shouldBe` False
      isBooleanValue (TmIf TmTrue TmFalse TmTrue) `shouldBe` False

  describe "isNumericValue" $ do

    it "returns True if argument is TmZero" $ do
      isNumericValue TmZero `shouldBe` True

    it "returns True if argument is TmSucc of numeric value" $ do
      isNumericValue (TmSucc TmZero) `shouldBe` True

    it "returns True if argument is TmPred of numeric value" $ do
      isNumericValue (TmPred TmZero) `shouldBe` True

    it "returns False otherwise" $ do
      isNumericValue TmTrue `shouldBe` False
      isNumericValue (TmSucc TmTrue) `shouldBe` False
      isNumericValue (TmIsZero TmZero) `shouldBe` False
      isNumericValue (TmIf TmTrue TmZero TmZero) `shouldBe` False
      isNumericValue (TmSucc (TmIsZero (TmPred TmZero))) `shouldBe` False
