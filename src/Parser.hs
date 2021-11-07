{-# LANGUAGE BlockArguments #-}

module Parser
  ( ParseResult,
    parse,
    runParser,
    Parser,
    Input,
    parseVarSym,
    parseConSym,
    parseQTyCon,
    parseTopDecl,
    parseDecls,
    parseDecl,
    parseIDecl,
    parseConstrs,
    parseConstr,
    parseFieldDecl,
    parseDeriving,
    parseFunLhs,
    parseRhs,
    parseGdRhs,
    parseGuards,
    parseGuard,
    parseExp,
    parseLExp,
    parseInfixExp,
    parseAExp,
    parseStmts,
    parseStmt,
    parseFBind,
    parsePat,
    parseLPat,
    parseAPat,
    parseGCon,
    parseVarOp,
    parseQOp,
  )
where

import Common (Error (..))
import Control.Applicative
import Data.List (intercalate)
import Data.Maybe (isJust)
import Elements
import ParserHelpers
import Tokens (KeywordToken (..), Token (..))

-- TODO ParseItem monad?

type ParseResult = Module

parse :: Input -> Either Error ParseResult
parse input = do
  (element, remainder) <- runParser parseModule input
  pure element

-- PARSE TERMINALS

parseLiteral :: Parser Literal
parseLiteral =
  Literal_Int <$> parseLitInteger
    <|> Literal_Float <$> parseLitFloat
    <|> Literal_Char <$> parseLitChar
    <|> Literal_String <$> parseLitString

parseQualifiedId :: Parser a -> (Maybe ModId -> a -> b) -> Parser b
parseQualifiedId pa con = do
  conIds <- zeroOrMoreSep Dot parseConId
  let modIdStr = (intercalate "." . map (\(ConId str) -> str)) conIds
  let maybeModId = if null modIdStr then Nothing else Just (ModId modIdStr)
  id <- pa
  pure $ con maybeModId id

parseQualifiedConId :: (Maybe ModId -> ConId -> a) -> Parser a
parseQualifiedConId con = do
  conIds <- oneOrMoreSep Dot parseConId
  let modIdStr = (intercalate "." . map (\(ConId str) -> str) . init) conIds
  let maybeModId = if null modIdStr then Nothing else Just (ModId modIdStr)
  let id = last conIds
  pure $ con maybeModId id

parseVarSym :: Parser VarSym
parseVarSym =
  VarSym
    <$> oneOfTokensAsString
      [ Tokens.Varsym,
        -- Not part of VarSym because has special meaning:
        Tokens.Dot
      ]

parseConSym :: Parser ConSym
parseConSym = ConSym <$> parseTokenToString Tokens.Consym

parseVarId :: Parser VarId
parseVarId = VarId <$> parseTokenToString Tokens.ValueName

parseConId :: Parser ConId
parseConId = ConId <$> parseTokenToString Tokens.TypeName

parseTyVar :: Parser TyVar
parseTyVar = parseVarId

parseTyCon :: Parser TyCon
parseTyCon = parseConId

parseTyCls :: Parser TyCls
parseTyCls = parseConId

parseModId :: Parser ModId
parseModId = do
  conIds <- oneOrMoreSep Dot parseConId
  let strings = map (\(ConId str) -> str) conIds
  let modId = intercalate "." strings
  pure $ ModId modId

parseQVarId :: Parser QVarId
--parseQVarId = QVarId <$> optional (parseModId `followedBy` Dot) <*> parseVarId
parseQVarId = parseQualifiedId parseVarId QVarId

parseQConId :: Parser QConId
parseQConId = parseQualifiedConId QConId

parseQTyCon :: Parser QTyCon
parseQTyCon = parseQualifiedConId QTyCon

parseQTyCls :: Parser QTyCls
parseQTyCls = parseQualifiedConId QTyCls

parseQVarSym :: Parser QVarSym
parseQVarSym = parseQualifiedId parseVarSym QVarSym

parseQConSym :: Parser QConSym
parseQConSym = parseQualifiedId parseConSym QConSym

parseLitInteger :: Parser LitInteger
parseLitInteger = LitInteger <$> parseTokenToString Tokens.IntegerLiteral

parseLitFloat :: Parser LitFloat
parseLitFloat = LitFloat <$> parseTokenToString Tokens.FloatLiteral

parseLitChar :: Parser LitChar
parseLitChar = LitChar <$> parseTokenToString Tokens.CharLiteral

parseLitString :: Parser LitString
parseLitString = LitString <$> parseTokenToString Tokens.StringLiteral

-- NON-TERMINAL PARSERS

parseModule :: Parser Module
parseModule = do
  parseKeyword Tokens.Module
  modId <- parseModId
  exports <- optional parseExports
  parseKeyword Tokens.Where
  body <- parseBody
  --  parseToken Tokens.EOF -- comment out to allow partial parse
  pure $ Elements.Module modId exports body

parseBody :: Parser Body
parseBody = do
  parseToken LeftBrace
  impDecls <- parseImpDecls
  optional (parseToken SemiColon)
  topDecls <- parseTopDecls
  parseToken RightBrace
  return $ Body impDecls topDecls

parseImpDecls :: Parser ImpDecls
parseImpDecls = zeroOrMoreSep SemiColon parseImpDecl

parseExports :: Parser Exports
parseExports = do
  parseToken LeftParan
  exports <- zeroOrMoreSep Comma parseExport
  optional (parseToken Comma)
  parseToken RightParan
  return exports

parseExport :: Parser Export
parseExport =
  Export_QVar <$> parseQVar
    <|> Export_QTyCon <$> parseQTyCon <*> ParserHelpers.either parseAll (tupled parseCName)
    <|> Export_QTyCls <$> parseQTyCls <*> ParserHelpers.either parseAll (tupled parseQVar)
    <|> Export_Module <$> parseModId

parseImpDecl :: Parser ImpDecl
parseImpDecl = do
  parseKeyword Tokens.Import
  qualified <- isJust <$> optional (parseString "qualified")
  modId <- parseModId
  modIdAs <- optional $ parseModId `precededByString` "as"
  impSpec <- optional $ parseImpSpec
  pure $ ImpDecl qualified modId modIdAs impSpec

parseImpSpec :: Parser ImpSpec
parseImpSpec =
  ImpSpec <$> tupled' parseImport
    <|> ImpSpec_Hiding <$> tupled' parseImport `precededByString` "hiding"

parseImport :: Parser Import
parseImport =
  Import_Var <$> parseVar
    <|> Import_TyCon <$> parseTyCon <*> optional (ParserHelpers.either parseAll (tupled parseCName))
    <|> Import_TyCls <$> parseTyCls <*> optional (ParserHelpers.either parseAll (tupled parseVar))

parseCName :: Parser CName
parseCName = ParserHelpers.either parseVar parseCon

parseTopDecls :: Parser TopDecls
parseTopDecls = zeroOrMoreSep Tokens.SemiColon parseTopDecl

parseTopDecl :: Parser TopDecl
parseTopDecl =
  Keyword Tokens.Type `precedes` (TopDecl_Type <$> parseSimpleType <*> parseType `precededBy` Equals)
    <|> Keyword Data `precedes` (TopDecl_Data <$> optional (parseContext `followedBy` DoubleRightArrow) <*> parseSimpleType <*> optional (parseConstrs `precededBy` Equals) <*> optional parseDeriving)
    <|> Keyword NewType `precedes` (TopDecl_NewType <$> optional (parseContext `followedBy` DoubleRightArrow) <*> parseSimpleType <*> parseNewConstr `precededBy` Equals <*> optional parseDeriving)
    <|> Keyword Tokens.Class `precedes` (TopDecl_Class <$> optional (parseSContext `followedBy` DoubleRightArrow) <*> parseTyCls <*> parseTyVar <*> optional (parseCDecls `precededBy` Keyword Where))
    <|> Keyword Instance `precedes` (TopDecl_Instance <$> optional (parseSContext `followedBy` DoubleRightArrow) <*> parseQTyCls <*> parseInst <*> optional (parseIDecls `precededBy` Keyword Where))
    <|> Keyword Default `precedes` (TopDecl_Default <$> betweenBraces (zeroOrMoreSep Comma parseType))
    --  | TopDecl_Foreign [FDecl] -- TODO foreign
    <|> TopDecl_Decl <$> parseDecl

parseDecls :: Parser Decls
parseDecls = betweenBraces (zeroOrMoreSep Tokens.SemiColon parseDecl)

parseDecl :: Parser Decl
parseDecl =
  Decl_GenDecl <$> parseGenDecl
    <|> Decl_FunLhs <$> parseFunLhs <*> parseRhs
    <|> Decl_Pat <$> parsePat <*> parseRhs

parseCDecls :: Parser CDecls
parseCDecls = betweenBraces (zeroOrMoreSep Tokens.SemiColon parseCDecl)

parseCDecl :: Parser CDecl
parseCDecl =
  CDecl_GenDecl <$> parseGenDecl
    <|> CDecl_FunLhs <$> parseFunLhs <*> parseRhs
    <|> CDecl_Var <$> parseVar <*> parseRhs

parseIDecls :: Parser IDecls
parseIDecls = betweenBraces (zeroOrMoreSep Tokens.SemiColon parseIDecl)

parseIDecl :: Parser IDecl
parseIDecl =
  IDecl_FunLhs <$> parseFunLhs <*> parseRhs
    <|> IDecl_Var <$> parseVar <*> parseRhs

parseGenDecl :: Parser GenDecl
parseGenDecl =
  ( do
      vars <- zeroOrMore parseVar
      parseToken DoubleColon
      maybeContext <- optional (parseContext `followedBy` Tokens.DoubleRightArrow)
      type_ <- parseType
      pure $ GenDecl_TypeSig vars maybeContext type_
  )
    <|> ( do
            fixity <- parseFixity
            maybeInteger <- optional parseLitInteger
            ops <- parseOps
            pure $ GenDecl_Fixity fixity maybeInteger ops
        )

parseOps :: Parser Ops
parseOps = oneOrMoreSep Comma parseOp

parseVars :: Parser Vars
parseVars = oneOrMoreSep Comma parseVar

parseFixity :: Parser Fixity
parseFixity =
  Fixity_Infixl <$ parseToken (Keyword Infixl)
    <|> Fixity_Infixr <$ parseToken (Keyword Infixr)
    <|> Fixity_Infix <$ parseToken (Keyword Infix)

parseType :: Parser Type
parseType = do
  bTypes <- oneOrMoreSep Tokens.SingleArrow parseBType
  return (Elements.Type bTypes)

parseBType :: Parser BType
parseBType = do
  aTypes <- oneOrMore parseAType
  return (BType aTypes)

parseAType :: Parser AType
parseAType =
  AType_GTyCon <$> parseGTyCon
    <|> AType_TyVar <$> parseTyVar
    <|> AType_Tuple <$> tupled parseType
    <|> AType_List <$> betweenBrackets parseType
    <|> AType_ParanCon <$> betweenParans parseType

parseGTyCon :: Parser GTyCon
parseGTyCon =
  GTyCon_QTyCon <$> parseQTyCon
    <|> GTyCon_Unit <$ parseTokens [LeftParan, RightParan]
    <|> GTyCon_ListCon <$ parseTokens [LeftBracket, RightBracket]
    <|> GTyCon_FunCon <$ betweenParans (parseToken SingleArrow)
    <|> GTyCon_TupleCon <$ betweenParans (zeroOrMore (parseToken Comma))

parseContext :: Parser Context
parseContext =
  Context <$> betweenParans (zeroOrMoreSep Tokens.Comma parseClass)
    <|> Context <$> mkList parseClass

parseClass :: Parser Class
parseClass = do
  qTyCls <- parseQTyCls
  tyVar <- parseTyVar
  return (Elements.Class qTyCls tyVar)

parseSContext :: Parser SContext
parseSContext =
  SContext <$> betweenParans (zeroOrMoreSep Tokens.Comma parseSimpleClass)
    <|> SContext <$> mkList parseSimpleClass

parseSimpleClass :: Parser SimpleClass
parseSimpleClass = SimpleClass <$> parseQTyCls <*> parseTyVar

parseSimpleType :: Parser SimpleType
parseSimpleType = SimpleType <$> parseTyCon <*> zeroOrMore parseTyVar

parseConstrs :: Parser Constrs
parseConstrs = oneOrMoreSep Pipe parseConstr

parseConstr :: Parser Constr
parseConstr =
  -- Different order than Haskell2010 syntax reference because of specificity of parsers
  Constr_FieldDecls <$> parseCon <*> betweenBraces (zeroOrMoreSep Comma parseFieldDecl)
    -- TODO infix conop
    <|> Constr_ATypes <$> parseCon <*> zeroOrMore (parseAType `precededByOpt` Exclamation)

parseNewConstr :: Parser NewConstr
parseNewConstr =
  NewConstr_AType <$> parseCon <*> parseAType
    <|> ( do
            con <- parseCon
            parseToken LeftBrace
            var <- parseVar
            parseToken DoubleColon
            type_ <- parseType
            parseToken RightBrace
            return (NewConstr_Type con var type_)
        )

parseFieldDecl :: Parser FieldDecl
parseFieldDecl = do
  vars <- parseVars
  parseToken DoubleColon
  type_ <- ParserHelpers.either parseType (parseAType `precededBy` Exclamation)
  return (FieldDecl vars type_)

parseDeriving :: Parser Deriving
parseDeriving = do
  parseKeyword Tokens.Deriving
  dClasses <- betweenParans (zeroOrMoreSep Tokens.Comma parseDClass) <|> mkList parseDClass
  return (Elements.Deriving dClasses)

parseDClass :: Parser DClass
parseDClass = DClass <$> parseQTyCls

parseInst :: Parser Inst
parseInst =
  Inst_GTyCon <$> parseGTyCon
    <|> ( do
            parseToken LeftParan
            gTyCon <- parseGTyCon
            tyVars <- zeroOrMore parseTyVar
            parseToken RightParan
            return (Inst_GQTyConTyVars gTyCon tyVars)
        )
    <|> Inst_TyVarsTuple <$> tupled parseTyVar
    <|> Inst_TyVarsList <$> listed parseTyVar
    <|> ( do
            parseToken LeftParan
            tyVar1 <- parseTyVar
            parseToken SingleArrow
            tyVar2 <- parseTyVar
            parseToken RightParan
            return (Inst_TyVar2 tyVar1 tyVar2)
        )

parseFunLhs :: Parser FunLhs
parseFunLhs =
  FunLhs_Var <$> parseVar <*> oneOrMore parseAPat
    <|> FunLhs_Pat <$> parsePat <*> parseVarOp <*> parsePat
    <|> FunLhs_Fun <$> betweenParans parseFunLhs <*> oneOrMore parseAPat

parseRhs :: Parser Rhs
parseRhs =
  Rhs_Exp <$> parseExp `precededBy` Equals <*> optional (parseDecls `precededBy` Keyword Where)
    <|> Rhs_GdRhs <$> parseGdRhs <*> optional (parseDecls `precededBy` Keyword Where)

parseGdRhs :: Parser GdRhs
parseGdRhs = do
  guards <- parseGuards
  parseToken Tokens.Equals
  exp <- parseExp
  maybeGdRhs <- optional parseGdRhs
  pure $ GdRhs guards exp maybeGdRhs

parseGuards :: Parser Guards
parseGuards = do
  parseToken Pipe
  guards <- oneOrMoreSep Comma parseGuard
  pure $ guards

parseGuard :: Parser Guard
parseGuard =
  Guard_Pat <$> parsePat <*> parseInfixExp `precededBy` LeftArrow
    <|> Guard_Decls <$> parseDecls `precededBy` Keyword Let
    <|> Guard_Infix <$> parseInfixExp

parseExp :: Parser Exp
parseExp = Exp_InfixExp <$> parseInfixExp

-- TODO expression type signature

parseInfixExp :: Parser InfixExp
parseInfixExp =
  InfixExp_Infix <$> parseLExp <*> parseQOp <*> parseInfixExp
    <|> InfixExp_Neg <$> parseInfixExp `precededBy` Dash
    <|> InfixExp_LExp <$> parseLExp

parseLExp :: Parser LExp
parseLExp =
  LExp_Lambda <$> oneOrMore parseAPat `precededBy` Backslash <*> parseExp `precededBy` SingleArrow
    <|> LExp_Let <$> parseDecls `precededBy` Keyword Let <*> parseExp `precededBy` Keyword In
    <|> LExp_IfElse <$> parseExp `precededBy` Keyword If <* optional (parseToken SemiColon) <*> parseExp `precededBy` Keyword Then <* optional (parseToken SemiColon) <*> parseExp `precededBy` Keyword Else
    <|> LExp_Case <$> parseExp `precededBy` Keyword Case <*> betweenBraces parseAlts `precededBy` Keyword Of
    <|> LExp_Do <$> betweenBraces parseStmts `precededBy` Keyword Do
    <|> LExp_FExp <$> parseFExp

parseFExp :: Parser FExp
parseFExp = do
  f <- parseAExp
  args <- zeroOrMore parseAExp
  pure $ FExp f args

parseAExp :: Parser AExp
parseAExp =
  -- Different order than Haskell2010 syntax reference because of specificity of parsers
  AExp_ParanExp <$> betweenParans parseExp
    <|> AExp_Tuple <$> tupled parseExp
    <|> AExp_List <$> listed parseExp
    <|> betweenBrackets (AExp_ArithSeq <$> parseExp <*> optional (parseExp `precededBy` Comma) <* parseToken DoubleDot <*> optional parseExp)
    <|> betweenBrackets (AExp_ListComp <$> parseExp <* parseToken Pipe <*> oneOrMoreSep Comma parseQual)
    <|> betweenParans (AExp_LeftSect <$> parseInfixExp <*> parseQOp)
    <|> betweenParans (AExp_RightSect <$> parseQOp <*> parseInfixExp) -- TODO ignore ⟨-⟩
    <|> AExp_LabelCon <$> parseQCon <*> betweenBraces (zeroOrMoreSep Comma parseFBind)
    -- TODO labeled update
    <|> AExp_QVar <$> parseQVar
    <|> AExp_GCon <$> parseGCon
    <|> AExp_Lit <$> parseLiteral

parseQual :: Parser Qual
parseQual =
  Qual_Gen <$> parsePat <*> (parseExp `precededBy` LeftArrow)
    <|> Qual_Local <$> (parseDecls `precededBy` Keyword Let)
    <|> Qual_Guard <$> parseExp

parseAlts :: Parser Alts
parseAlts = oneOrMoreSep SemiColon parseAlt

parseAlt :: Parser Alt
parseAlt =
  Alt_Exp <$> parsePat <*> parseExp `precededBy` SingleArrow <*> optional (parseDecls `precededBy` Keyword Where)
    <|> Alt_GdPat <$> parsePat <*> parseGdPat <*> optional (parseDecls `precededBy` Keyword Where)
    <|> Alt_Empty <$ parseNop

parseGdPat :: Parser GdPat
parseGdPat = do
  guards <- parseGuards
  parseToken SingleArrow
  exp <- parseExp
  maybeGdPat <- optional parseGdPat
  pure $ GdPat guards exp maybeGdPat

parseStmts :: Parser Stmts
parseStmts = do
  stmts <- zeroOrMore parseStmt
  exp <- trailingSemicolon parseExp
  pure $ Stmts stmts exp

parseStmt :: Parser Stmt
parseStmt =
  Stmt_Exp <$> parseExp `followedBy` SemiColon
    <|> Stmt_Pat <$> parsePat <*> between parseExp LeftArrow SemiColon
    <|> Stmt_Decls <$> between parseDecls (Keyword Let) SemiColon
    <|> Stmt_Empty <$ parseToken SemiColon

parseFBind :: Parser FBind
parseFBind = FBind <$> parseQVar <* parseToken Tokens.Equals <*> parseExp

parsePat :: Parser Pat
parsePat =
  Pat_Infix <$> parseLPat <*> parseQConOp <*> parsePat
    <|> Pat_LPat <$> parseLPat

parseLPat :: Parser LPat
parseLPat =
  -- Different order than Haskell2010 syntax reference because of specificity of parsers
  LPat_GCon <$> parseGCon <*> oneOrMore parseAPat
    <|> LPat_NegLit <$> ParserHelpers.either parseLitInteger parseLitFloat `precededBy` Dash
    <|> LPat_APat <$> parseAPat

parseAPat :: Parser APat
parseAPat =
  APat_Var <$> parseVar <*> optional (parseAPat `precededBy` Tokens.At)
    <|> APat_GCon <$> parseGCon
    <|> APat_QCon <$> parseQCon <*> zeroOrMore parseFPat
    <|> APat_Lit <$> parseLiteral
    <|> APat_Wildcard <$ parseToken Underscore
    <|> APat_Paran <$> betweenParans parsePat
    <|> APat_Tuple <$> tupled parsePat
    <|> APat_List <$> listed parsePat
    <|> APat_Irrefut <$> parseAPat `precededBy` Tilde

parseFPat :: Parser FPat
parseFPat = FPat <$> parseQVar <*> parsePat

parseGCon :: Parser GCon
parseGCon =
  GCon_Unit <$ parseTokens [LeftParan, RightParan]
    <|> GCon_List <$ parseTokens [LeftBracket, RightBracket]
    <|> GCon_Tuple <$ betweenParans (zeroOrMore (parseToken Comma))
    <|> GCon_QCon <$> parseQCon

parseVar :: Parser Var
parseVar =
  Var_VarId <$> parseVarId
    <|> Var_VarSym <$> betweenParans parseVarSym

parseQVar :: Parser QVar
parseQVar =
  QVar_QVarId <$> parseQVarId
    <|> QVar_QVarSym <$> betweenParans parseQVarSym

parseCon :: Parser Con
parseCon =
  Con_ConId <$> parseConId
    <|> Con_ConSym <$> betweenParans parseConSym

parseQCon :: Parser QCon
parseQCon =
  QCon_QCondId <$> parseQConId
    <|> QCon_QConSym <$> betweenParans parseQConSym

parseVarOp :: Parser VarOp
parseVarOp =
  VarOp_VarSym <$> parseVarSym
    <|> VarOp_VarId <$> betweenTicks parseVarId

parseQVarOp :: Parser QVarOp
parseQVarOp =
  QVarOp_QVarSym <$> parseQVarSym
    <|> QVarOp_QVarId <$> betweenTicks parseQVarId

parseConOp :: Parser ConOp
parseConOp =
  ConOp_ConSym <$> parseConSym
    <|> ConOp_ConId <$> betweenTicks parseConId

parseQConOp :: Parser QConOp
parseQConOp =
  QConOp_GConSym <$> parseGConSym
    <|> QConOp_QConId <$> betweenTicks parseQConId

parseOp :: Parser Op
parseOp =
  Op_VarOp <$> parseVarOp
    <|> Op_ConOp <$> parseConOp

parseQOp :: Parser QOp
parseQOp =
  QOp_QVarOp <$> parseQVarOp
    <|> QOp_QConOp <$> parseQConOp

parseGConSym :: Parser GConSym
parseGConSym =
  GConSym_Colon <$ parseToken Colon
    <|> GConSym_QConSym <$> parseQConSym
