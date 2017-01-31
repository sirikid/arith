module SpecUtils
  ( module Test.Hspec
  , shouldReturn
  , shouldThrow
  ) where

import Control.Monad.Except (MonadError, throwError)
import Test.Hspec hiding (shouldReturn, shouldThrow)

infix 1 `shouldReturn`, `shouldThrow`

-- FIXME Call stack
shouldReturn :: (Applicative f, Eq (f a), Show (f a)) => f a -> a -> Expectation
shouldReturn action expectedValue = action `shouldBe` pure expectedValue

-- FIXME Call stack
shouldThrow :: (MonadError e m, Eq (m a), Show (m a)) => m a -> e -> Expectation
shouldThrow action expectedError = action `shouldBe` throwError expectedError

-- TODO shouldNotReturn, shouldNotThrow
