{-# LANGUAGE BlockArguments #-}

module Parser
  ( parse,
    runParser,
    ParseError,
    Parser,
    Input,
    parseConSym,
    parseDecls,
    parseDecl,
    parseFunLhs,
    parseRhs,
    parseVarSym,
    parseGdRhs,
    parseGuards,
    parseExp,
    parseLExp,
    parseInfixExp,
    parseTopDecl,
    parseConstrs,
    parseConstr,
    parseDeriving,
    parsePat,
    parseLPat,
    parseAPat,
    parseGCon,
  )
where

import Common (Coordinates)
import Control.Applicative
import Control.Monad
import Data.List (intercalate, (\\))
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace (traceM)
import Elements
import Lexer (ScanItem (..))
import Text.Printf (printf)
import Tokens (KeywordToken (..), Token (..), isDashes, isReservedOp)

parse :: Input -> Either ParseError Module
parse input = do
  (elem_, rem_) <- parseRecursive input
  traceM ("remainder: " ++ show rem_)
  pure elem_

parseRecursive :: Input -> Either ParseError (Module, Input)
parseRecursive = runParser parseModule

type Input = [ScanItem Token]

newtype Parser a = Parser {runParser :: Input -> Either ParseError (a, Input)}

instance Functor Parser where
  fmap fab pa = Parser $ fmap fn . runParser pa
    where
      fn (a, input) = (fab a, input)

instance Applicative Parser where
  pure match = Parser $ \input -> Right (match, input)
  p1 <*> p2 =
    Parser $ \input -> do
      (fab, notParsed) <- runParser p1 input
      (a, notParsed') <- runParser p2 notParsed
      return (fab a, notParsed')

unit :: a -> Parser a
unit match = Parser $ \input -> Right (match, input)

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ runParser p >=> fn
  where
    fn (match, remaining) = runParser (f match) remaining

instance Monad Parser where
  return = unit
  (>>=) = bind

failure :: Parser a
failure = Parser $ \s -> Left $ ParseError (71, 0) "Failure"

-- Run first parser. If fail, run second parser
option :: Parser a -> Parser a -> Parser a
option pa1 pa2 = Parser $ \s ->
  case runParser pa1 s of
    res@(Right _) -> res
    Left err1 ->
      case runParser pa2 s of
        res@(Right _) -> res
        Left err2 ->
          if errorLoc err1 >= errorLoc err2
            then Left err1
            else Left err2

instance Alternative Parser where
  empty = failure
  (<|>) = option

data ParseError = ParseError Coordinates String deriving (Eq, Show)

errorLoc :: ParseError -> Coordinates
errorLoc (ParseError c _) = c

errorMsg :: ParseError -> String
errorMsg (ParseError _ s) = s

-- PARSERS HELPERS

fail :: Either ParseError a
fail = Left $ ParseError (0, 0) "Fail"

parseFail :: Parser a
parseFail = Parser $ const Parser.fail

parseNothing :: Parser (Maybe a)
parseNothing = Parser $ \s -> Right (Nothing, s)

parseNop :: Parser Nop
parseNop = Parser $ \s -> Right (Nop, s)

parseItem :: Token -> Parser (ScanItem Token)
parseItem expTok = Parser fn
  where
    fn (si : xs)
      | scanTok si == expTok = Right (si, xs)
      | otherwise = Left $ ParseError (0, 0) (printf "Could not parse item %s as expected token %s" (show si) (show expTok))
    fn [] = Left $ ParseError (0, 0) "Could not find next scan item"

parseToken :: Token -> Parser Token
parseToken expTok = scanTok <$> parseItem expTok

parseTokenAsString :: Token -> Parser String
parseTokenAsString expTok = scanStr <$> parseItem expTok

parseTokens :: [Token] -> Parser [Token]
parseTokens = mapM parseToken

parseKeyword :: KeywordToken -> Parser KeywordToken
parseKeyword keyword = parser >>= toKWParser
  where
    parser = parseToken (Keyword keyword)
    toKWParser tok =
      case tok of
        Keyword kw -> Parser (\input -> Right (kw, input))
        _ -> parseFail

either :: Parser a -> Parser b -> Parser (Either a b)
either pa pb = Left <$> pa <|> Right <$> pb

oneOfTokens :: [Token] -> Parser Token
oneOfTokens tokens = foldl1 (<|>) (map parseToken tokens)

oneOfTokensAsString :: [Token] -> Parser String
oneOfTokensAsString tokens = foldl1 (<|>) (map parseTokenAsString tokens)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore pa = Parser $ \s -> fn s []
  where
    fn (x : xs) matches =
      case nextResult of
        Left (ParseError _ _) -> Right (matches, x : xs)
        Right (match, remaining) -> fn remaining (matches ++ [match])
      where
        nextResult = runParser pa (x : xs)
    fn [] matches = Right (matches, [])

oneOrMore :: Parser a -> Parser [a]
oneOrMore pa = do
  x <- pa
  xs <- zeroOrMore pa
  pure (x : xs)

zeroOrMoreSep :: Token -> Parser a -> Parser [a]
zeroOrMoreSep t pa = oneOrMoreSep t pa <|> pure []

oneOrMoreSep :: Token -> Parser a -> Parser [a]
oneOrMoreSep t pa = do
  x <- pa
  xs <- zeroOrMore (pa `precededBy` t)
  pure (x : xs)

-- zeroOrMoreSep with mandatory first separator
zeroOrMoreSep2 :: Token -> Parser a -> Parser [a]
zeroOrMoreSep2 t pa = zeroOrMore (pa `precededBy` t)

-- oneOrMoreSep with mandatory first separator
oneOrMoreSep2 :: Token -> Parser a -> Parser [a]
oneOrMoreSep2 t pa = oneOrMore (pa `precededBy` t)

tupled :: Parser a -> Parser [a]
tupled pa = do
  parseToken Tokens.LeftParan
  elems <- oneOrMoreSep Tokens.Comma pa
  parseToken Tokens.RightParan
  pure elems

-- Allows extra comma at the end
tupled' :: Parser a -> Parser [a]
tupled' pa = do
  parseToken Tokens.LeftParan
  elems <- oneOrMoreSep Tokens.Comma pa
  optional $ parseToken Tokens.Comma
  parseToken Tokens.RightParan
  pure elems

listed :: Parser a -> Parser [a]
listed pa = do
  parseToken Tokens.LeftBracket
  elems <- oneOrMoreSep Tokens.Comma pa
  parseToken Tokens.RightBracket
  pure elems

mkList :: Parser a -> Parser [a]
mkList = fmap (\x -> [x])

betweenParans :: Parser a -> Parser a
betweenParans pa = between pa Tokens.LeftParan Tokens.RightParan

betweenBraces :: Parser a -> Parser a
betweenBraces pa = between pa Tokens.LeftBrace Tokens.RightBrace

betweenBrackets :: Parser a -> Parser a
betweenBrackets pa = between pa Tokens.LeftBracket Tokens.RightBracket

betweenTicks :: Parser a -> Parser a
betweenTicks pa = between pa Tokens.BackTick Tokens.BackTick

parseEmpty :: a -> Parser a
parseEmpty a = Parser $ \s -> Right (a, s)

precededBy :: Parser a -> Token -> Parser a
precededBy pa t = do
  parseToken t
  pa

precededByOpt :: Parser a -> Token -> Parser a
precededByOpt pa t = do
  optional $ parseToken t
  pa

followedBy :: Parser a -> Token -> Parser a
followedBy pa t = do
  result <- pa
  parseToken t
  return result

followedByOpt :: Parser a -> Token -> Parser a
followedByOpt pa t = do
  result <- pa
  optional $ parseToken t
  return result

precedes :: Token -> Parser a -> Parser a
precedes t pa = do
  parseToken t
  pa

between :: Parser a -> Token -> Token -> Parser a
between pa l r = do
  parseToken l
  a <- pa
  parseToken r
  return a

trailingSemicolon :: Parser a -> Parser a
trailingSemicolon pa = do
  a <- pa
  optional (parseToken SemiColon)
  return a

parseAll :: Parser All
parseAll = do
  betweenParans (parseToken DoubleDot)
  return All

failIf :: (a -> Bool) -> Parser a -> Parser a
failIf cond pa = do
  x <- pa
  if cond x then pure x else failure

-- PARSE TERMINALS

parseLiteral :: Parser Literal
parseLiteral =
  Literal_Int <$> parseInteger
    <|> Literal_Float <$> parseFloat
    <|> Literal_Char <$> parseChar
    <|> Literal_String <$> parseString

getNext :: Parser Token
getNext = Parser fn
  where
    fn (ScanItem {scanTok = tok} : xs) = Right (tok, xs)
    fn [] = Left $ ParseError (0, 0) "Could not get next"

parseSymbol :: Parser Symbol
parseSymbol = parseAscSymbol

symbolList = ascSymbolList -- TODO add unicode symbols

ascSymbolList =
  [ Exclamation,
    Hash,
    Dollar,
    Percent,
    Ampersand,
    Asterisk,
    Plus,
    Dot,
    Divide,
    LeftAngle,
    Equals,
    RightAngle,
    Question,
    At,
    Backslash,
    Caret,
    Pipe,
    Dash,
    Tilde,
    Colon
  ]

parseAscSymbol :: Parser Symbol
parseAscSymbol = Symbol <$> oneOfTokens ascSymbolList

parseSymbolString :: Parser String
parseSymbolString = oneOfTokensAsString ascSymbolList

parseVarSym :: Parser VarSym
parseVarSym = do
  firstSymbol <- oneOfTokensAsString (symbolList \\ [Tokens.Colon])
  remSymbols <- zeroOrMore parseSymbolString
  let symbols = concat (firstSymbol : remSymbols)
  if isReservedOp symbols || isDashes symbols then failure else pure (VarSym symbols)

parseConSym :: Parser ConSym
parseConSym = do
  firstSymbol <- parseTokenAsString Tokens.Colon
  remSymbols <- zeroOrMore parseSymbolString
  let symbols = concat (firstSymbol : remSymbols)
  if isReservedOp symbols then failure else pure (ConSym symbols)

parseVarId :: Parser VarId
parseVarId = VarId <$> parseTokenAsString Tokens.ValueName

parseConId :: Parser ConId
parseConId = ConId <$> parseTokenAsString Tokens.TypeName

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
parseQVarId = parseVarId

parseQConId :: Parser QConId
parseQConId = parseConId

parseQTyCon :: Parser QTyCon
parseQTyCon = parseTyCon

parseQTyCls :: Parser QTyCls
parseQTyCls = parseTyCls

parseQVarSym :: Parser QVarSym
parseQVarSym = parseVarSym

parseQConSym :: Parser QConSym
parseQConSym = do
  maybeModId <- optional parseModId `followedBy` Dot
  let modIdStr = fromMaybe "" (fmap (\(ModId str) -> str ++ ".") maybeModId)
  (ConSym conSymStr) <- parseConSym
  pure $ QConSym (modIdStr ++ conSymStr)

parseInteger :: Parser LitInteger
parseInteger = LitInteger <$> parseTokenAsString Tokens.IntegerLiteral

parseFloat :: Parser LitFloat
parseFloat = LitFloat <$> parseTokenAsString Tokens.FloatLiteral

parseChar :: Parser LitChar
parseChar = LitChar <$> parseTokenAsString Tokens.CharLiteral

parseString :: Parser LitString
parseString = LitString <$> parseTokenAsString Tokens.StringLiteral

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
    <|> Export_QTyCon <$> parseQTyCon <*> Parser.either parseAll (tupled parseCName)
    <|> Export_QTyCls <$> parseQTyCls <*> Parser.either parseAll (tupled parseQVar)
    <|> Export_Module <$> parseModId

parseImpDecl :: Parser ImpDecl
parseImpDecl = do
  parseKeyword Tokens.Import
  qualified <- isJust <$> optional (parseKeyword Qualified)
  modId <- parseModId
  modIdAs <- optional $ parseModId `precededBy` Keyword As
  impSpec <- optional $ parseImpSpec
  pure $ ImpDecl qualified modId modIdAs impSpec

parseImpSpec :: Parser ImpSpec
parseImpSpec =
  ImpSpec <$> tupled' parseImport
    <|> ImpSpec_Hiding <$> tupled' parseImport `precededBy` Keyword Hiding

parseImport :: Parser Import
parseImport =
  Import_Var <$> parseVar
    <|> Import_TyCon <$> parseTyCon <*> optional (Parser.either parseAll (tupled parseCName))
    <|> Import_TyCls <$> parseTyCls <*> optional (Parser.either parseAll (tupled parseVar))

parseCName :: Parser CName
parseCName = Parser.either parseVar parseCon

parseTopDecls :: Parser TopDecls
parseTopDecls = zeroOrMoreSep Tokens.SemiColon parseTopDecl

parseTopDecl :: Parser TopDecl
parseTopDecl =
  Keyword Tokens.Type `precedes` (TopDecl_Type <$> parseSimpleType <*> parseType `precededBy` Equals)
    <|> Keyword Data `precedes` (TopDecl_Data <$> optional (parseContext `followedBy` DoubleArrow) <*> parseSimpleType <*> optional (parseConstrs `precededBy` Equals) <*> optional parseDeriving)
    <|> Keyword NewType `precedes` (TopDecl_NewType <$> optional (parseContext `followedBy` DoubleArrow) <*> parseSimpleType <*> parseNewConstr `precededBy` Equals <*> optional parseDeriving)
    <|> Keyword Tokens.Class `precedes` (TopDecl_Class <$> optional (parseSContext `followedBy` DoubleArrow) <*> parseTyCls <*> parseTyVar <*> optional (parseCDecls `precededBy` Keyword Where))
    <|> Keyword Instance `precedes` (TopDecl_Instance <$> optional (parseSContext `followedBy` DoubleArrow) <*> parseQTyCls <*> parseInst <*> optional (parseIDecls `precededBy` Keyword Where))
    <|> Keyword Default `precedes` (TopDecl_Default <$> betweenBraces (zeroOrMoreSep Comma parseType))
    --  | TopDecl_Foreign [FDecl] -- TODO
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
      maybeContext <- optional (parseContext `followedBy` Tokens.DoubleArrow)
      type_ <- parseType
      pure $ GenDecl_TypeSig vars maybeContext type_
  )
    <|> ( do
            fixity <- parseFixity
            maybeInteger <- optional parseInteger
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
  Constr_ATypes <$> parseCon <*> zeroOrMore (parseAType `precededByOpt` Exclamation)
    -- TODO
    <|> Constr_FieldDecls <$> parseCon <*> betweenBraces (zeroOrMoreSep Comma parseFieldDecl)

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
  type_ <- Parser.either parseType (parseAType `precededBy` Exclamation)
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
  AExp_QVar <$> parseQVar
    <|> AExp_GCon <$> parseGCon
    <|> AExp_Lit <$> parseLiteral
    <|> AExp_ParanExp <$> betweenParans parseExp
    <|> AExp_Tuple <$> tupled parseExp
    <|> AExp_List <$> listed parseExp
    <|> betweenParans (AExp_LeftSect <$> parseInfixExp <*> parseQOp)
    <|> betweenParans (AExp_RightSect <$> parseQOp <*> parseInfixExp) -- TODO ignore ⟨-⟩
    <|> AExp_LabelCon <$> parseQCon <*> zeroOrMore parseFBind

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
parseFBind = do
  qVar <- parseQVar
  parseToken Tokens.Equals
  exp <- parseExp
  return (FBind qVar exp)

parsePat :: Parser Pat
parsePat =
  Pat_Infix <$> parseLPat <*> parseQConOp <*> parsePat
    <|> Pat_LPat <$> parseLPat

parseLPat :: Parser LPat
-- order changed to parse smallest possibility last
parseLPat =
  LPat_GCon <$> parseGCon <*> oneOrMore parseAPat
  <|> LPat_NegLit <$> Parser.either parseInteger parseFloat `precededBy` Dash
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
parseVar = Var_VarId <$> parseVarId
  <|> Var_VarSym <$> betweenParans parseVarSym

parseQVar :: Parser QVar
parseQVar = QVar_QVarId <$> parseQVarId
  <|> QVar_QVarSym <$> betweenParans parseQVarSym

parseCon :: Parser Con
parseCon = Con_ConId <$> parseConId
  <|> Con_ConSym <$> betweenParans parseConSym

parseQCon :: Parser QCon
parseQCon = QCon_QCondId <$> parseQConId
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
