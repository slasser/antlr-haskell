{-# LANGUAGE QuasiQuotes, TemplateHaskell, ScopedTypeVariables, DataKinds #-}
module Language.ANTLR4.Boot.Quote
( antlr4, ProdElem(..), g4_decls
) where
import Prelude hiding (exp, init)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (nub, elemIndex)
import Data.Char (toLower, toUpper, isLower, isUpper)
import Data.Maybe (fromJust, catMaybes)

import qualified Debug.Trace as D

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift, Exp(..))
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Language.Haskell.Meta as LHM

import Control.Monad (mapM)
import qualified Language.ANTLR4.Boot.Syntax as G4S
import qualified Language.ANTLR4.Boot.Parser as G4P
import qualified Language.ANTLR4.Regex  as G4R
import Text.ANTLR.Grammar
import Text.ANTLR.Parser (AST(..), StripEOF(..))
import Text.ANTLR.Pretty
import qualified Text.ANTLR.Lex.Tokenizer as T
import qualified Text.ANTLR.LR as LR
import qualified Text.ANTLR.Allstar as ALL

import Text.ANTLR.Set (Set(..))
import qualified Text.ANTLR.Set as Set
import qualified Text.ANTLR.Lex.Regex as R

--trace s = D.trace   ("[Language.ANTLR4.Boot.Quote] " ++ s)
--traceM s = D.traceM ("[Language.ANTLR4.Boot.Quote] " ++ s)

trace s x = x
traceM s x = x

haskellParseExp :: (Monad m) => String -> m TH.Exp
haskellParseExp s = case LHM.parseExp s of
  Left err    -> error err
  Right expTH -> return expTH

haskellParseType :: (Monad m) => String -> m TH.Type
haskellParseType s = case LHM.parseType s of
  Left err   -> trace s (error err)
  Right tyTH -> return tyTH

type2returnType :: TH.Type -> TH.Type
type2returnType = let
    
    t2rT :: TH.Type -> TH.Type
    t2rT (ForallT xs ys t) = t2rT t
    t2rT ((AppT (AppT ArrowT from) to)) = t2rT to
    t2rT t@(VarT _)        = t
    t2rT t@(AppT ListT as) = t
    t2rT t@(ConT _)        = t
    t2rT t@(AppT (ConT _) _) = t
    t2rT x = error (show x)

  in t2rT

info2returnType :: Info -> TH.Type
info2returnType i = let
  
  in case i of
      (VarI _ t _) -> type2returnType t
      _ -> error (show i)

--trace s = id
--traceM = return

antlr4 :: QuasiQuoter
antlr4 =  QuasiQuoter
  (error "parse exp")
  (error "parse pattern")
  (error "parse type")
  aparse --(error "parse decl")

-- e.g. Named ("Num", "Int") where 'Num' was a G4 lexeme and 'Int' was given
-- as a directive specifying the desired type to read (must instance Read).
data LexemeType =
    Literal Int           -- A literal lexeme somewhere in the grammar, e.g. ';'
  | AString               -- Type was unspecified in the G4 lexeme or specified as a String
  | Named String TH.TypeQ -- Type was specified as a directive in the G4 lexeme

--   parser in quasiquotation monad
aparse :: String -> TH.Q [TH.Dec]
aparse input = do
 -- TODO: replace bad error showing with
 --       debugging information (filename, line #, column) in parser
 loc <- TH.location
 let fileName = TH.loc_filename loc
 let (line,column) = TH.loc_start loc

 case G4P.parseANTLR fileName line column input of
   Left err -> unsafePerformIO $ fail $ show err
   Right x  -> g4_decls x

g4_decls :: [G4S.G4] -> TH.Q [TH.Dec] -- exp :: G4
g4_decls ast = let

    -- Ordered (arbitrary) list of the terminal literals found in production
    -- rules of the grammar:
    --terminalLiterals :: [String]
    --terminalLiterals = nub $ concatMap getTLs ast

    -- Get Terminal Literals
    --getTLs :: G4S.G4 -> [String]
    --getTLs G4S.Prod{G4S.patterns = ps} = concatMap (justLiterals . G4S.alphas) ps
    --getTLs _ = []

    --justLiterals :: [G4S.ProdElem] -> [String]
    --justLiterals [] = []
    --justLiterals (

    -- A list of all the G4 literal terminals scattered across production rules
    terminalLiterals :: [String]
    terminalLiterals = (nub $ concatMap getTerminals ast)

    -- A list of all the terminals in the grammar (both literal G4 terminals and
    -- G4 lexical terminals)
    terminals :: [String]
    terminals = terminalLiterals ++ lexemeNames

    -- A list of all the G4 lexeme names specified in the grammar
    lexemeNames :: [String]
    lexemeNames = map fst lexemeTypes

    nonterms  :: [String]
    nonterms  = nub $ concatMap getNTs ast

    -- Find all terminals *literals* in a production like '(' and ')' and ';'
    justTerms :: [G4S.ProdElem] -> [String]
    justTerms [] = []
    justTerms ((G4S.GTerm s) : as) = s : justTerms as
    justTerms (_:as) = justTerms as

    -- Find all nonterminals in a production like 'exp' and 'decl'
    justNonTerms :: [G4S.ProdElem] -> [String]
    justNonTerms [] = []
    justNonTerms (G4S.GNonTerm s:as)
      | (not . null) s && isLower (head s) = s : justNonTerms as
      | otherwise = justNonTerms as
    justNonTerms (_:as) = justNonTerms as

    -- Find all terminal literals in a G4 grammar rule like '(' and ')' and ';'
    getTerminals :: G4S.G4 -> [String]
    getTerminals G4S.Prod{G4S.patterns = ps} = concatMap (justTerms . G4S.alphas) ps
    getTerminals _ = []

    -- Find all the nonterminals referenced in the production(s) of the given grammar rule
    getNTs :: G4S.G4 -> [String]
    getNTs G4S.Prod{G4S.pName = pName, G4S.patterns = ps} = pName : concatMap (justNonTerms . G4S.alphas) ps
    getNTs _ = []

    -- Find the (first) name of the grammar
    grammarName :: [G4S.G4] -> String
    grammarName [] = error "Grammar missing a name"
    grammarName (G4S.Grammar{G4S.gName = gName}:_) = gName
    grammarName (_:xs) = grammarName xs

    gName = grammarName ast

    ntDataName = gName ++ "NTSymbol"
    tDataName  = gName ++ "TSymbol"

    -- Things Symbols must derive:
    symbolDerives = cxt $ map (conT . mkName)
      [ "Eq", "Ord", "Show", "Hashable", "Generic", "Bounded", "Enum"]

    ntDataDeclQ :: DecQ
    ntDataDeclQ =
      dataD (cxt [])
      (mkName ntDataName)
      []
      Nothing
      (map (\s -> normalC (mkName $ "NT_" ++ s) []) nonterms)
      symbolDerives
    
    -- E.g. ['(', ')', ';', 'exp', 'decl']
    allLexicalSymbols :: [String]
    allLexicalSymbols = map (lookupTName "") terminalLiterals ++ lexemeNames

    -- E.g. [('(', Literal 0), (')', Literal 1), (';', Literal 2), ('exp',
    -- AString), ('decl', AString')]
    allLexicalTypes :: [(String, LexemeType)]
    allLexicalTypes = (map lookupLiteralType terminalLiterals) ++ lexemeTypes

    -- E.g. [('(', Literal 0), ...]
    lookupLiteralType :: String -> (String, LexemeType)
    lookupLiteralType s =
      case s `elemIndex` terminalLiterals of
        Nothing -> undefined
        Just i  -> (s, Literal i)

    tDataDeclQ =
      dataD (cxt [])
        (mkName tDataName)  
        []
        Nothing 
        (map (\s -> normalC (mkName s) []) (map ("T_" ++) allLexicalSymbols))
        --(\s -> normalC (mkName $ lookupTName "T_" s) []) lexemes) ++ (lexemeNames "T_"))
        symbolDerives

    ntConT = conT $ mkName ntDataName
    tConT  = conT $ mkName tDataName

    -- e.g. [('UpperID', AString), ('SetChar', Named String)]
    lexemeTypes :: [(String, LexemeType)]
    lexemeTypes = let
        lN :: G4S.G4 -> [(String, LexemeType)]
        lN (G4S.Lex{G4S.lName = lName, G4S.pattern = G4S.LRHS{G4S.directive = Nothing}}) = [(lName, AString)]
        lN (G4S.Lex{G4S.lName = lName, G4S.pattern = G4S.LRHS{G4S.directive = Just s}})
          | s == "String"     = [(lName, AString)]
          | null s            = [(lName, AString)] -- quirky G4 parser
          | isUpper (head s)  = [(lName, Named s (conT $ mkName s))]
          | otherwise         = [(lName, Named s (info2returnType <$> reify (mkName s)))]
        lN _ = []
      in concatMap lN ast
      --map (\s -> normalC (mkName s) []) lN'

    lookupTName :: String -> String -> String
    lookupTName pfx s = pfx ++
      (case s `elemIndex` terminalLiterals of
        Nothing -> s
        Just i  -> show i)

    strBangType = (defBang, conT $ mkName "String")

    mkCon   = conE . mkName . mkUpper
    mkConNT = conE . mkName . ("NT_" ++)

    toElem :: G4S.ProdElem -> TH.ExpQ
    toElem (G4S.GTerm s)    = [| $(mkCon "T")  $(mkCon $ lookupTName "T_" s) |] -- $(return $ LitE $ StringL s)) |]
    toElem (G4S.GNonTerm s)
      | (not . null) s && isLower (head s) = [| $(mkCon "NT") $(mkConNT s) |]
      | otherwise = toElem (G4S.GTerm s)

    mkProd :: String -> [TH.ExpQ] -> TH.ExpQ
    mkProd n es = [| $(mkCon "Production") $(conE $ mkName $ "NT_" ++ n) ($(mkCon "Prod") $(mkCon "Pass") $(listE es)) |]

    getProds :: [G4S.G4] -> [TH.ExpQ]
    getProds [] = []
    getProds (G4S.Prod {G4S.pName = n, G4S.patterns = ps}:xs)
      = map (mkProd n . map toElem . G4S.alphas) ps ++ getProds xs
    getProds (_:xs) = getProds xs

    -- The first NonTerminal in the grammar (TODO: head of list)
    s0 :: TH.ExpQ
    s0 = conE $ mkName $ "NT_" ++ head nonterms

    grammar gTy = [| (defaultGrammar $(s0) :: $(return gTy))
      { ns = Set.fromList [minBound .. maxBound :: $(ntConT)]
      , ts = Set.fromList [minBound .. maxBound :: $(tConT)]
      , ps = $(listE $ getProds ast)
      } |]

    grammarTy = [t| forall s. Grammar s $(ntConT) $(tConT) |]

    mkLower [] = []
    mkLower (a:as) = toLower a : as

    mkUpper [] = []
    mkUpper (a:as) = toUpper a : as

    {----------------------- Tokenizer -----------------------}

    tokenNameTypeQ = tySynD (mkName "TokenName") [] (conT $ mkName tDataName)
    
    defBang = bang noSourceUnpackedness noSourceStrictness

    lexemeValueDerives = cxt $ map (conT . mkName)
      ["Show", "Ord", "Eq", "Generic", "Hashable"]

    -- 
    lexemeTypeConstructors = let
        lTC (i, lex@(G4S.Lex{G4S.lName = lName, G4S.pattern = G4S.LRHS{G4S.directive = Just d}}))
          | null lName       = error $ "null lexeme name: " ++ show lex
          | null d           = Just $ normalC (mkName $ "V_" ++ lName) [bangType defBang (conT $ mkName "String")]
          | isUpper $ head d = Just $ normalC (mkName $ "V_" ++ lName) [bangType defBang (conT $ mkName d)]
          | otherwise        = Just $ do
              info <- reify $ mkName d
              normalC (mkName $ "V_" ++ lName) [bangType defBang (return $ info2returnType info)]
            --Just $ [|| $$(haskellParseExp d) ||] --error $ "unimplemented use of function in G4 directive: " ++ show d
        lTC _ = Nothing
      in   ((catMaybes $ map lTC (zip [0 .. length ast - 1] ast))
        ++ (map (\s -> normalC (mkName $ lookupTName "V_" s) []) terminalLiterals))

    tokenValueTypeQ =
      dataD (cxt []) (mkName "TokenValue") [] Nothing
      lexemeTypeConstructors
      lexemeValueDerives

    mkTyVar s f = return $ f $ mkName s

    lookupTokenFncnDecl = let
        lTFD t = clause [litP $ stringL t]
                  (normalB $ [| T.Token $(conE $ mkName   $ lookupTName "T_" t)
                                        $(conE $ mkName   $ lookupTName "V_" t)
                                        $(litE $ integerL $ fromIntegral $ length t) |])
                  []
      in funD (mkName "lookupToken")
        (  map lTFD terminalLiterals
        ++ [clause [varP $ mkName "s"]
            (normalB $ [| error ("Error: '" ++ s ++ "' is not a token") |])
            []]
        )

    -- Construct the function that takes in a lexeme (string) and the token name
    -- (T_*) and constructs a token value type instance using 'read' where
    -- appropriate based on the directives given in the grammar.
    lexeme2ValueQ lName = let
        
        l2VQ (_, Literal i) =
          clause [varP lName, conP (mkName $ "T_" ++ show i) []]
          (normalB [| $(conE $ mkName $ "V_" ++ show i) |]) []
        l2VQ (s, AString)   =
          clause [varP lName, conP (mkName $ "T_" ++ s) []]
          (normalB [| $(conE $ mkName $ "V_" ++ s) $(varE lName) |]) []
        l2VQ (s, Named n t)
          | isLower (head n) =
              clause [varP lName, conP (mkName $ "T_" ++ s) []]
              (normalB [| $(conE $ mkName $ "V_" ++ s) (trace $(varE lName) ($(varE $ mkName n) $(varE lName) :: $t)) |]) []
          | otherwise =
              clause [varP lName, conP (mkName $ "T_" ++ s) []]
              (normalB [| $(conE $ mkName $ "V_" ++ s) (trace $(varE lName) (read $(varE lName) :: $t)) |]) []
              
              --info <- reify $ mkName d
              --normalC (mkName $ "V_" ++ lName) [bangType defBang (return $ info2returnType info)]
      
      in funD (mkName "lexeme2value") (map l2VQ allLexicalTypes)

    -- Convert a G4 regex into the backend regex type (for constructing token
    -- recognizers as DFAs):
    convertRegex :: (Show c) => G4R.Regex c -> R.Regex c
    convertRegex = let
        cR G4R.Epsilon       = R.Epsilon
        cR (G4R.Literal [])  = R.Epsilon
        cR (G4R.Literal [c]) = R.Symbol c
        cR (G4R.Literal cs)  = R.Literal cs
        cR (G4R.Union rs)    = R.MultiUnion $ map cR rs
        cR (G4R.Concat rs)   = R.Concat $ map cR rs
        cR (G4R.Kleene r)    = R.Kleene $ cR r
        cR (G4R.PosClos r)   = R.PosClos $ cR r
        cR (G4R.Question r)  = R.Question $ cR r
        cR (G4R.CharSet cs)  = R.Class cs
        cR (G4R.Negation (G4R.CharSet cs)) = R.NotClass cs
        cR (G4R.Negation (G4R.Literal s)) = R.NotClass s
        cR r@(G4R.Negation _) = error $ "unimplemented: " ++ show r
        cR (G4R.Named _)    = error "unimplemented"
      in cR

    -- Make the list of tuples containing regexes, one for each terminal.
    mkRegexesQ = let
        mkLitR :: String -> ExpQ
        mkLitR s = [| ($( conE $ mkName $ lookupTName "T_" s)
                        , $(lift $ convertRegex $ G4R.Literal s)) |]

        mkLexR :: G4S.G4 -> Maybe ExpQ
        mkLexR (G4S.Lex{G4S.lName = lName, G4S.pattern = G4S.LRHS{G4S.regex = r}}) = Just
          [| ($(conE $ mkName $ lookupTName "T_" lName), $(lift $ convertRegex r)) |]
        mkLexR _ = Nothing
      in valD (varP $ mkName $ mkLower $ gName ++ "Regexes")
          (normalB $ listE (map mkLitR terminalLiterals ++ (catMaybes $ map mkLexR ast)))
          []

    prettyTFncnQ fncnName = let
        pTFLit lexeme =
          clause [conP (mkName $ lookupTName "T_" lexeme) []]
          (normalB [| pStr $(litE $ stringL $ "'" ++ lexeme ++ "'") |])
          []

        pTFName lexeme = 
          clause [conP (mkName $ lookupTName "T_" lexeme) []]
          (normalB [| pStr $(litE $ stringL $ lexeme) |])
          []
      in funD fncnName (map pTFLit terminalLiterals ++ map pTFName lexemeNames)

    prettyVFncnQ fncnName = let
        pVFLit lexeme =
          clause [conP (mkName $ lookupTName "V_" lexeme) []]
          (normalB [| pStr $(litE $ stringL $ "'" ++ lexeme ++ "'") |])
          []

        pVFName lexeme = 
          clause [conP (mkName $ lookupTName "V_" lexeme) [varP (mkName "v")]]
          (normalB [| pChr '\'' >> prettify v >> pChr '\'' |])
          []
      in funD fncnName (map pVFLit terminalLiterals ++ map pVFName lexemeNames)
    
    -- Pattern matches on an AST to produce a Maybe DataType
    ast2DTFncnsQ nameAST = let
        
        a2d G4S.Lex{G4S.lName  = _A, G4S.pattern = G4S.LRHS{G4S.directive = dir}}
          = Just  [ funD (mkName $ "ast2" ++ _A)
                    [ clause  [ conP (mkName "Leaf")
                                [ conP (mkName $ "T.Token")
                                  [ wildP
                                  , conP (mkName $ lookupTName "V_" _A)
                                    [ varP $ mkName "t"]
                                  , wildP]]]
                              (normalB (varE $ mkName "t"))
                              []
                    ]
                  ]
        {-
        a2d G4S.Lex{G4S.lName  = _A, G4S.pattern = G4S.LRHS{G4S.directive = Just s}}
          | s == "String" = Just [funD (mkName $ "ast2" ++ _A) [ clause [] (normalB (varE $ mkName "id")) [] ]]
          | null s        = Just [funD (mkName $ "ast2" ++ _A) [ clause [] (normalB (varE $ mkName "id")) [] ]]
          | otherwise     = Just [funD (mkName $ "ast2" ++ _A) [ clause [] (normalB (varE $ mkName s)) [] ]]
        -}
        a2d G4S.Prod{G4S.pName = _A, G4S.patterns = ps} = let

          fncnName = mkName $ "ast2" ++ _A

          mkConP (G4S.GNonTerm nt)
            -- Some nonterminals are really terminal tokens (regular expressions):
            | isUpper (head nt)     = conP (mkName "T")  [conP (mkName $ lookupTName "T_" nt) []]
            | otherwise             = conP (mkName "NT") [conP (mkName $ "NT_" ++ nt) []]
          mkConP (G4S.GTerm    t)   = conP (mkName "T")  [conP (mkName $ lookupTName "T_" t) []]

          nonTermVars as = map (\(G4S.GNonTerm t) -> t) (filter G4S.isGNonTerm as)
          
          justStr (G4S.GNonTerm s) = s
          justStr (G4S.GTerm s)    = s

          vars as = catMaybes
                    [ if G4S.isGNonTerm a
                        then Just (mkName $ "v" ++ show i ++ "_" ++ justStr a, varE $ mkName $ "ast2" ++ justStr a)
                        else Nothing
                    | (i, a) <- zip [0 .. length as] as
                    ]

          astListPattern as = listP $
                [ if G4S.isGNonTerm a
                    then varP  $ mkName $ "v" ++ show i ++ "_" ++ justStr a
                    else wildP
                | (i, a) <- zip [0 .. length as] as
                ]

          astAppRec b (varName, recName) = appE b (appE recName $ varE varName)

          clauses = [ clause  [ [p| AST $(conP (mkName $ "NT_" ++ _A) [])
                                     $(listP $ map mkConP as)
                                     $(astListPattern as)
                                |]
                                     -- $(listP $ map (varP . fst) $ vars as)
                              ]
                        (case (dir, vars as) of
                          (Just d, vs)    ->
                                      if isUpper (head d)
                                        then normalB $ foldl astAppRec (conE $ mkName d) vs
                                        else normalB $ foldl astAppRec (varE $ mkName d) vs
                          (Nothing, [])   -> normalB $ tupE []
                          (Nothing, [(v0,rec)]) -> normalB $ appE rec (varE v0)
                          (Nothing, vs)         -> normalB $ tupE $ map (\(vN,rN) -> appE rN $ varE vN) vs
                        ) []
                    | G4S.PRHS{G4S.alphas = as, G4S.pDirective = dir} <- ps
                    ] ++
                    [ clause [ [p| ast2 |] ] (normalB [| error (show ast2) |]) [] ]
          
          retType = let
            rT G4S.PRHS{G4S.alphas = as, G4S.pDirective = dir}
              = case (dir, vars as) of
                  (Just d, vs) ->
                    if isUpper (head d)
                      then (do i <- reify $ mkName d
                               (case i of
                                        DataConI _ t n -> return $ type2returnType t
                                        VarI n t _     -> return t
                                        TyConI (DataD _ n _ _ _ _) -> conT n
                                        other          -> error $ show other))
                      else info2returnType <$> reify (mkName d)
                  (Nothing, [])         -> tupleT 0
                  (Nothing, [(v0,rec)]) -> tupleT 0
                  (Nothing, vs)         -> tupleT $ length vs
            in rT (head ps)

          fncnSig
            = do rT <- retType
                 (case rT of
                    ForallT vs c t  -> forallT vs (cxt []) [t| $(conT nameAST) -> $(return t) |]
                    t               -> forallT [] (cxt []) [t| $(conT nameAST) -> $(return t) |])

          in Just $ [ --sigD fncnName fncnSig
                      funD fncnName clauses
                    ]
        a2d _ = Nothing
      in (concat . catMaybes . map a2d) ast

  -- terminaLiterals, lexemeNames

  -- IMPORTANT: Creating type variables in two different haskell type
  -- quasiquoters with the same variable name produces two (uniquely) named type
  -- variables. In order to achieve the same type variable you need to run one
  -- in the Q monad first then pass the resulting type to other parts of the
  -- code that need it (thus capturing the type variable).
  in do 
        
        let tokVal    = mkName "TokenValue"
            tokName   = mkName "TokenName"
            ntSym     = mkName ntDataName
            tSym      = mkName tDataName
            nameAST   = mkName (mkUpper gName ++ "AST")
            nameToken = mkName (mkUpper gName ++ "Token")
            nameDFAs  = mkName (mkLower gName ++ "DFAs")
            name      = mkName $ mkLower (gName ++ "Grammar")
        prettyTFncnName <- newName "prettifyT"
        prettyValueFncnName <- newName "prettifyValue"
        
        ntDataDecl <- ntDataDeclQ
        tDataDecl  <- tDataDeclQ
        gTy    <- grammarTy
        gTySig <- sigD name (return gTy)
        g      <- grammar gTy
        gFunD  <- funD name [clause [] (normalB (return g)) []]
        prettyNT:_     <- [d| instance Prettify $(ntConT) where prettify = rshow |]
        prettyT:_      <- [d| instance Prettify $(tConT) where prettify = $(varE prettyTFncnName) |]
        prettyValue:_  <- [d| instance Prettify $(conT tokVal) where prettify = $(varE prettyValueFncnName) |]
        lookupTokenD   <- lookupTokenFncnDecl

        tokenNameType  <- tokenNameTypeQ
        tokenValueType <- tokenValueTypeQ
        
        let lName = mkName "l"
        lexeme2Value   <- lexeme2ValueQ lName

        regexes <- mkRegexesQ
        let dfasName    = mkName $ mkLower gName ++ "DFAs"
        let regexesE    = varE $ mkName $ mkLower gName ++ "Regexes"
        dfas <- funD dfasName [clause [] (normalB [| map (fst &&& regex2dfa . snd) $(regexesE) |]) []]

        astDecl <-tySynD nameAST   [] [t| AST $(conT ntSym) $(conT nameToken) |]
        tokDecl <- tySynD nameToken [] [t| T.Token $(conT tSym) $(conT tokVal) |]
        
        decls <-
          [d| instance Ref $(conT ntSym) where
                type Sym $(conT ntSym) = $(conT ntSym)
                getSymbol = id
              
              tokenize :: String -> [$(conT nameToken)] --T.Token $(conT tokName) $(conT tokVal)]
              tokenize = T.tokenize $(varE nameDFAs) lexeme2value

              slrParse :: [$(conT nameToken)] -> LR.LRResult () $(conT ntSym) (StripEOF (Sym $(conT nameToken))) $(conT nameToken) $(conT nameAST)
              slrParse = (LR.slrParse $(varE name) event2ast)
              
              --glrParse :: [$(conT nameToken)] -> LR.LRResult $(conT ntSym) (StripEOF (Sym $(conT nameToken))) $(conT nameToken) $(conT nameAST)
              glrParse :: ($(conT tokName) -> Bool) -> [Char] -> LR.LR1Result $(conT ntSym) 
                                                                                (StripEOF (Sym $(conT nameToken)))
                                                                                Char
                                                                                $(conT nameAST)
              glrParse filterF = (LR.glrParseInc $(varE name) event2ast (T.tokenizeInc filterF $(varE nameDFAs) lexeme2value))
         
              instance ALL.Token $(conT nameToken) where
                type Label $(conT nameToken) = StripEOF (Sym $(conT nameToken))
                getLabel = fromJust . stripEOF . getSymbol

                type Literal $(conT nameToken) = $(conT tokVal)
                getLiteral = T.tokenValue

              allstarParse :: [$(conT nameToken)] -> Either String $(conT nameAST)
              allstarParse inp = ALL.parse inp (ALL.NT $(s0)) (ALL.atnOf $(varE name)) True
          |]
        
        prettyTFncn <- prettyTFncnQ prettyTFncnName
        prettyVFncn <- prettyVFncnQ prettyValueFncnName

        ast2DTFncns <- sequence $ ast2DTFncnsQ nameAST

        return $
          [ ntDataDecl, tDataDecl
          , gTySig
          , gFunD
          , tokenNameType, tokenValueType
          , prettyTFncn, prettyVFncn
          , prettyNT, prettyT, prettyValue
          , lookupTokenD
          , lexeme2Value
          , regexes
          , dfas, astDecl, tokDecl
          ] ++ decls ++ ast2DTFncns

