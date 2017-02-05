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

evaluateAndShow :: String -> String
evaluateAndShow = either id show . (tokenize >=> parse >=> evaluate)
