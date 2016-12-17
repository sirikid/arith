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
      tokenize "\n\rtrue\t\0" `shouldBe` tokenize "true"
