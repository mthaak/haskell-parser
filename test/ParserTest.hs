module ParserTest
  ( parserTests,
  )
where

import Lexer
import Parser
import Test.HUnit
import Tokens

-- TODO allow supplying tokens, and not also string and coordinates
canParse :: Parser a -> Input -> Assertion
canParse pa input = case runParser pa input of
  Right (_, []) -> assertBool "" True
  Right (_, _) -> assertBool "" False
  Left _ -> assertBool "" False

test_parseConSym :: Test
test_parseConSym =
  TestCase
    ( canParse
        parseConSym
        [si (0, 0) ":+" Consym]
    )

test_parseDecls :: Test
test_parseDecls =
  TestCase
    ( canParse
        parseDecls
        [ScanItem (0, 0) "{" LeftBrace, ScanItem (16, 5) "x" ValueName, ScanItem (16, 7) "=" Equals, ScanItem (16, 9) "1" IntegerLiteral, ScanItem (0, 0) ";" SemiColon, ScanItem (17, 5) "y" ValueName, ScanItem (17, 7) "=" Equals, ScanItem (17, 9) "2" IntegerLiteral, ScanItem (0, 0) "}" RightBrace]
    )

test_parseDecl_Pat :: Test
test_parseDecl_Pat =
  TestCase
    ( canParse
        parseDecl
        [si (0, 0) "x" ValueName, si (0, 0) "=" Equals, si (0, 0) "3" IntegerLiteral]
    )

test_parseDecl_FunLhs :: Test
test_parseDecl_FunLhs =
  TestCase
    ( canParse
        parseDecl
        [si (0, 0) "add" ValueName, si (0, 0) "x" ValueName, si (0, 0) "=" Equals, si (0, 0) "x" ValueName, si (0, 0) "+" Varsym, si (0, 0) "3" IntegerLiteral]
    )

test_parseDecl_GenDecl :: Test
test_parseDecl_GenDecl =
  TestCase
    ( canParse
        parseDecl
        [si (0, 0) "add" ValueName, si (0, 0) "::" DoubleColon, si (0, 0) "Num" TypeName, si (0, 0) "a" ValueName, si (0, 0) "=>" DoubleRightArrow, si (0, 0) "a" ValueName, si (0, 0) "->" SingleArrow, si (0, 0) "a" ValueName]
    )

test_parseFunLhs_Var :: Test
test_parseFunLhs_Var =
  TestCase
    ( canParse
        parseFunLhs
        [si (0, 0) "add" ValueName, si (0, 0) "x" ValueName]
    )

test_parseFunLhs_Pat :: Test
test_parseFunLhs_Pat =
  TestCase
    ( canParse
        parseFunLhs
        [si (0, 0) "add" ValueName, si (0, 0) "[" LeftBracket, si (0, 0) "]" RightBracket]
    )

test_parseGdRhs :: Test
test_parseGdRhs =
  TestCase
    ( canParse
        parseGdRhs
        [si (21, 3) "|" Pipe, si (21, 5) "scanTok" ValueName, si (21, 14) "=" Equals, si (21, 16) "1" IntegerLiteral]
    )

test_parseGuards :: Test
test_parseGuards =
  TestCase
    ( canParse
        parseGuards
        [si (21, 3) "|" Pipe, si (21, 5) "scanTok" ValueName]
    )

test_parseExp_Literal :: Test
test_parseExp_Literal =
  TestCase
    ( canParse
        parseExp
        [si (0, 0) "1" IntegerLiteral]
    )

test_parseExp_DoubleEquals :: Test
test_parseExp_DoubleEquals =
  TestCase
    ( canParse
        parseExp
        [ScanItem (20, 47) "ab" ValueName, ScanItem (20, 50) "==" Varsym, ScanItem (20, 53) "2" IntegerLiteral]
    )

test_parseRhs_Exp1 :: Test
test_parseRhs_Exp1 =
  TestCase
    ( canParse
        parseRhs
        [si (0, 0) "=" Equals, si (0, 0) "3" IntegerLiteral]
    )

test_parseRhs_Exp2 :: Test
test_parseRhs_Exp2 =
  TestCase
    ( canParse
        parseRhs
        [si (0, 0) "=" Equals, si (0, 0) "x" ValueName, si (0, 0) "+" Varsym, si (0, 0) "3" IntegerLiteral]
    )

test_parseVarSym :: Test
test_parseVarSym =
  TestCase
    ( canParse
        parseVarSym
        [si (0, 0) ">=>" Varsym]
    )

test_parseLExp :: Test
test_parseLExp =
  TestCase
    ( canParse
        parseLExp
        [si (0, 0) "(" LeftParan, si (0, 0) "+" Varsym, si (0, 0) "1" IntegerLiteral, si (0, 0) ")" RightParan]
    )

test_parseInfixExp :: Test
test_parseInfixExp =
  TestCase
    ( canParse
        parseInfixExp
        [si (0, 0) "Just" TypeName, si (0, 0) "1" IntegerLiteral]
    )

test_ParseConstrs :: Test
test_ParseConstrs =
  TestCase
    ( canParse
        parseConstrs
        [si (0, 0) "Module" TypeName, si (0, 0) "|" Pipe, si (0, 0) "Where" TypeName, si (0, 0) "|" Pipe, si (0, 0) "Data" TypeName]
    )

test_ParseConstr :: Test
test_ParseConstr =
  TestCase
    ( canParse
        parseConstr
        [si (0, 0) "Module" TypeName]
    )

test_ParseTopDecl_Data :: Test
test_ParseTopDecl_Data =
  TestCase
    ( canParse
        parseTopDecl
        [si (0, 0) "data" (Keyword Data), si (0, 0) "KeywordToken" TypeName, si (0, 0) "=" Equals, si (0, 0) "Module" TypeName, si (0, 0) "|" Pipe, si (0, 0) "Where" TypeName, si (0, 0) "|" Pipe, si (0, 0) "Data" TypeName, si (0, 0) "deriving" (Keyword Tokens.Deriving), si (0, 0) "(" LeftParan, si (0, 0) "Eq" TypeName, si (0, 0) "," Comma, si (0, 0) "Ord" TypeName, si (0, 0) "," Comma, si (0, 0) "Show" TypeName, si (0, 0) ")" RightParan]
    )

test_parseDeriving :: Test
test_parseDeriving =
  TestCase
    ( canParse
        parseDeriving
        [si (0, 0) "deriving" (Keyword Tokens.Deriving), si (0, 0) "(" LeftParan, si (0, 0) "Eq" TypeName, si (0, 0) "," Comma, si (0, 0) "Ord" TypeName, si (0, 0) "," Comma, si (0, 0) "Show" TypeName, si (0, 0) ")" RightParan]
    )

test_parsePat_LPat :: Test
test_parsePat_LPat =
  TestCase
    ( canParse
        parsePat
        [si (0, 0) "x" ValueName]
    )

test_parseLPat_GCon :: Test
test_parseLPat_GCon =
  TestCase
    ( canParse
        parseLPat
        [si (72,23) "Si" TypeName, si (72,26) "a" ValueName]
    )

test_parseAPat_Var :: Test
test_parseAPat_Var =
  TestCase
    ( canParse
        parseAPat
        [si (0, 0) "x" ValueName]
    )

test_parseAPat_head_tail :: Test
test_parseAPat_head_tail =
  TestCase
    ( canParse
        parseAPat
        [si (19, 11) "(" LeftParan, si (19, 12) "x" ValueName, si (19, 14) ":" Colon, si (19, 16) "xs" ValueName, si (19, 18) ")" RightParan]
    )
    

test_parseGCon_QCon :: Test
test_parseGCon_QCon =
  TestCase
    ( canParse
        parseGCon
        [si (72,23) "Si" TypeName]
    )

test_parseVarOp :: Test
test_parseVarOp =
  TestCase
    ( canParse
        parseVarOp
        [si (0,0) "++" Varsym]
    )

parserTests :: Test
parserTests =
  TestLabel
    "ParserTests"
    ( TestList
        [
          TestLabel "test_parseConSym" test_parseConSym,
          TestLabel "test_parseDecls" test_parseDecls,
          TestLabel "test_parseDecl_GenDecl" test_parseDecl_GenDecl,
          TestLabel "test_parseDecl_Pat" test_parseDecl_Pat,
          TestLabel "test_parseDecl_FunLhs" test_parseDecl_FunLhs,
          TestLabel "test_parseFunLhs_Var" test_parseFunLhs_Var,
          TestLabel "test_parseFunLhs_Pat" test_parseFunLhs_Pat,
          TestLabel "test_parseRhs_Exp1" test_parseRhs_Exp1,
          TestLabel "test_parseRhs_Exp2" test_parseRhs_Exp2,
          TestLabel "test_parseVarSym" test_parseVarSym,
          TestLabel "test_parseLExp" test_parseLExp,
          TestLabel "test_parseGdRhs" test_parseGdRhs,
          TestLabel "test_parseGuards" test_parseGuards,
          TestLabel "test_parseExp_Literal" test_parseExp_Literal,
          TestLabel "test_parseExp_DoubleEquals" test_parseExp_DoubleEquals,
          TestLabel "test_parseInfixExp" test_parseInfixExp,
          TestLabel "test_ParseTopDecl_Data" test_ParseTopDecl_Data,
          TestLabel "test_ParseConstrs" test_ParseConstrs,
          TestLabel "test_ParseConstr" test_ParseConstr,
          TestLabel "test_parseDeriving" test_parseDeriving,
          TestLabel "test_parsePat_LPat" test_parsePat_LPat,
          TestLabel "test_parseLPat_GCon" test_parseLPat_GCon,
          TestLabel "test_parseAPat_Var" test_parseAPat_Var,
          TestLabel "test_parseAPat_head_tail" test_parseAPat_head_tail,
          TestLabel "test_parseGCon_QCon" test_parseGCon_QCon,
          TestLabel "test_parseVarOp" test_parseVarOp
        ]
    )
