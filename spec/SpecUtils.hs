module SpecUtils
  ( module Test.Hspec
  , shouldReturn
  , shouldThrowError
  ) where

import Control.Monad.Except (throwError)
import Test.Hspec hiding (shouldReturn)

-- FIXME Call stack
-- TODO Rewrite in more generic way?

infix 1 `shouldReturn`
shouldReturn :: Eq a => Show a => Eq b => Show b => Either a b -> b -> Expectation
shouldReturn = flip ((flip shouldBe) . pure)

infix 1 `shouldThrowError`
shouldThrowError :: Eq a => Show a => Eq b => Show b => Either a b -> a -> Expectation
shouldThrowError = flip ((flip shouldBe) . throwError)
