module Arith.LexerSpec
  ( spec
  ) where

import Test.Hspec
import Arith.Lexer

spec :: Spec
spec = do
  describe "tokenize" $ do

    it "returns corresponding token for each keyword" $ do
      tokenize "true"    `shouldBe` Right [KwTrue  ]
      tokenize "false"   `shouldBe` Right [KwFalse ]
      tokenize "if"      `shouldBe` Right [KwIf    ]
      tokenize "then"    `shouldBe` Right [KwThen  ]
      tokenize "else"    `shouldBe` Right [KwElse  ]
      tokenize "0"       `shouldBe` Right [KwZero  ]
      tokenize "succ"    `shouldBe` Right [KwSucc  ]
      tokenize "pred"    `shouldBe` Right [KwPred  ]
      tokenize "is_zero" `shouldBe` Right [KwIsZero]

    it "ignores spaces before keywords" $ do
      tokenize " true" `shouldBe` tokenize "true"

    it "ignores spaces after keywords" $ do
      tokenize "true " `shouldBe` tokenize "true"

    it "ignores spaces around keywords" $ do
      tokenize "\n\rtrue\t\v" `shouldBe` tokenize "true"

    it "returns sequence of tokens for sequence of keywords" $ do
      tokenize "true false" `shouldBe` Right [KwTrue,KwFalse]
      tokenize "false true" `shouldBe` Right [KwFalse,KwTrue]
      -- NOTE: It is not a valid program, but we can tokenize it
      tokenize "if then else" `shouldBe` Right [KwIf,KwThen,KwElse]
