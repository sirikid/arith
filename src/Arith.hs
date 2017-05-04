module Arith
  ( module Arith.Interpreter
  , module Arith.Lexer
  , module Arith.Parser
  , evaluateAndShow
  ) where

import Arith.Interpreter
import Arith.Lexer
import Arith.Parser
import Control.Monad ((>=>))
import Data.Bifunctor (first)

evaluateAndShow :: String -> String
evaluateAndShow = either id show . (tokenizeExpression >=> parse >=> evaluate)
