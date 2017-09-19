module Main where
-- Allstar imports go here, e.g.:
-- import Text.ANTLR.Allstar.ATN (..)

import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck (Property, quickCheck, (==>))
import qualified Test.QuickCheck.Monadic as TQM

import Language.ANTLR4
import Text.ANTLR.Grammar

import Text.ANTLR.Parser (AST(..))
import qualified Text.ANTLR.Lex.Tokenizer as T

import Test.Language.ANTLR4.SExpr

test_sexpr_allstar =
  allstarParse (tokenize "'(1 2 3)")
  @?=
  Right (AST NT_r [] [])

main :: IO ()
main = defaultMainWithOpts
  [ testCase "test_sexpr_allstar" test_sexpr_allstar
  ] mempty

