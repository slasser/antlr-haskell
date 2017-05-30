{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies #-}
module Language.Chisel.Parser
  ( parse, ChiselNTS(..), ChiselTS, ChiselAST
  ) where

import Text.ANTLR.Allstar.Grammar
import Text.ANTLR.Parser
import qualified Text.ANTLR.LR1 as P
import Language.Chisel.Tokenizer
import qualified Text.ANTLR.Lex.Tokenizer as T
import qualified Text.ANTLR.Set as S
import Text.ANTLR.Set (Hashable(..), Generic(..))
import Text.ANTLR.Pretty

import Debug.Trace as D

data ChiselNTS = ChiselProd | ProdSimple | Magnitude | Alignment | Formals
  | Group | Tuple | Alt | Flags | SizeArith
  deriving (Eq, Ord, Enum, Show, Bounded, Hashable, Generic)

instance Prettify ChiselNTS where prettify = rshow

instance Ref ChiselNTS where
  type Sym ChiselNTS = ChiselNTS
  getSymbol = id

-- (Name, Value) is what goes in the leaf of the AST...
type ChiselTS  = Name
type ChiselAST = AST ChiselNTS ChiselToken

type ChiselToken = T.Token ChiselTS Value

chiselGrammar :: Grammar () ChiselNTS ChiselTS
chiselGrammar = G
  { ns = S.fromList [ChiselProd .. maxBound :: ChiselNTS]
  , ts = S.fromList [T_LowerID .. maxBound :: ChiselTS]
  , s0 = ChiselProd
  , ps =  [ Production ChiselProd $ Prod Pass [NT ProdSimple]
          , Production ChiselProd $ Prod Pass [T T_LParen, NT ProdSimple, T T_RParen]
          , Production ProdSimple $ Prod Pass
            [ T T_UpperID, NT Formals, NT Magnitude, NT Alignment, T T_Arrow, NT Group]
          , Production ProdSimple $ Prod Pass
            [ T T_UpperID, NT Formals, T T_Arrow, NT Group]
          , Production Formals $ Prod Pass [T T_LowerID]
          , Production Formals $ Prod Pass [T T_LowerID, NT Formals]
          , Production Magnitude $ Prod Pass
            [ T T_VerticalBar, T T_Pound, NT SizeArith, T T_VerticalBar ]
          , Production Magnitude $ Prod Pass
            [ T T_VerticalBar,          NT SizeArith, T T_VerticalBar ]
          , Production Alignment $ Prod Pass
            [ T T_AtSymbol, T T_LParen, NT SizeArith, T T_RParen ]
          , Production Group $ Prod Pass [NT Tuple]
          , Production Group $ Prod Pass [NT Alt]
          , Production Group $ Prod Pass [NT Flags]
          , Production Alt $ Prod Pass [T T_UpperID] -- TODO...
          , Production Tuple $ Prod Pass [] -- TODO...
          , Production Flags $ Prod Pass [] -- TODO...
          ]
  }

event2chisel :: ParseEvent ChiselAST ChiselNTS ChiselToken -> ChiselAST
event2chisel e = D.trace (pshow e) (event2ast e)

parse :: [ChiselToken] -> Maybe ChiselAST
parse = P.slrParse chiselGrammar event2chisel . filter (not . isWhitespace)
