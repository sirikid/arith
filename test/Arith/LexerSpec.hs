module Arith.LexerSpec
  ( spec
  ) where

import Arith.Lexer
import Test.Hspec

spec :: Spec
spec = do
  describe "tokenize" $ do
    it "splits a string into a list of tokens" $ do
      let string = "true false if then else zero succ pred is_zero"
      let tokens = [KwTrue,KwFalse,KwIf,KwThen,KwElse,KwZero,KwSucc,KwPred,KwIsZero]
      tokenize string `shouldBe` Right tokens

    context "when an empty string given" $ do
      it "returns an empty list" $ do
        tokenize "" `shouldBe` Right []

    -- FIXME: Context message
    context "when only whitespaces given" $ do
      it "returns an empty list" $ do
        tokenize " " `shouldBe` Right []
        tokenize "\n\r\t\v" `shouldBe` Right []

    context "when a single keyword given" $ do
      it "returns a single token" $ do
        tokenize "true"    `shouldBe` Right [KwTrue  ]
        tokenize "false"   `shouldBe` Right [KwFalse ]
        tokenize "if"      `shouldBe` Right [KwIf    ]
        tokenize "then"    `shouldBe` Right [KwThen  ]
        tokenize "else"    `shouldBe` Right [KwElse  ]
        tokenize "zero"    `shouldBe` Right [KwZero  ]
        tokenize "succ"    `shouldBe` Right [KwSucc  ]
        tokenize "pred"    `shouldBe` Right [KwPred  ]
        tokenize "is_zero" `shouldBe` Right [KwIsZero]

    context "when a sequence of keywords given" $ do
      it "returns a sequence of tokens" $ do
        tokenize "zero"            `shouldBe` Right [KwZero]
        tokenize "zero true"       `shouldBe` Right [KwZero,KwTrue]
        tokenize "zero true false" `shouldBe` Right [KwZero,KwTrue,KwFalse]

    context "when given string contains non-keyword" $ do
      it "fails with most left non-keyword" $ do
        tokenize "iff than els" `shouldBe` Left "iff"
        tokenize "if  than els" `shouldBe` Left "than"
        tokenize "if  then els" `shouldBe` Left "els"
