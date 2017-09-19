{-# LANGUAGE QuasiQuotes, DeriveAnyClass, DeriveGeneric, TypeFamilies
		, DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances #-}
module Test.Language.ANTLR4.SExpr where

import Text.ANTLR.Grammar
import Text.ANTLR.Parser
import qualified Text.ANTLR.LR as P
--import Language.Chisel.Tokenizer
import qualified Text.ANTLR.Lex.Tokenizer as T
import qualified Text.ANTLR.Set as S
import Text.ANTLR.Set (Hashable(..), Generic(..))
import Text.ANTLR.Pretty
import Control.Arrow ( (&&&) )
import Text.ANTLR.Lex.Regex

import Language.ANTLR4
import Language.ANTLR4.G4

[g4|

grammar sexpression;

sexpr
   : item* EOF
   ;

item
   : atom
   | list
   | LPAREN item DOT item RPAREN
   ;

list
   : LPAREN item* RPAREN
   ;

atom
   : STRING
   | SYMBOL
   | NUMBER
   | DOT
   ;


STRING
   : '"' ('\\' . | ~ ('\\' | '"'))* '"'
   ;


WHITESPACE
   : (' ' | '\n' | '\t' | '\r') + -> skip
   ;


NUMBER
   : ('+' | '-')? (DIGIT) + ('.' (DIGIT) +)?
   ;


SYMBOL
   : SYMBOL_START (SYMBOL_START | DIGIT)*
   ;


LPAREN
   : '('
   ;


RPAREN
   : ')'
   ;


DOT
   : '.'
   ;


fragment SYMBOL_START
   : ('a' .. 'z') | ('A' .. 'Z') | '+' | '-' | '*' | '/' | '.'
   ;


fragment DIGIT
   : ('0' .. '9')
;
|]

