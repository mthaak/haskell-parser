module ParserTest
  ( parserTests,
  )
where

import Lexer
import Parser
import Test.HUnit
import Tokens

canParse :: Parser a -> Input -> Assertion
canParse pa input = case runParser pa input of
  Left err -> assertBool ("Parse unexpectedly failed: " ++ show err) False
  Right (_, []) -> assertBool "Parse expectedly successful" True
  Right (_, _) -> assertBool "Parse failed: input not fully consumed" False

cannotParse :: Parser a -> Input -> Assertion
cannotParse pa input = case runParser pa input of
  Left err -> assertBool ("Parse expectedly failed: " ++ show err) True
  Right (_, []) -> assertBool "Parse unexpectedly successful" False
  Right (_, _) -> assertBool "Parse failed: input not fully consumed" True

test_parseVarSym :: Test
test_parseVarSym =
  TestCase
    ( canParse
        parseVarSym
        [si (0, 0) ">=>" Varsym]
    )

test_parseConSym :: Test
test_parseConSym =
  TestCase
    ( canParse
        parseConSym
        [si (0, 0) ":+" Consym]
    )

test_parseQTyCon :: Test
test_parseQTyCon =
  TestCase
    ( canParse
        parseQTyCon
        [si (36, 7) "Common" TypeName, si (36, 13) "." Dot, si (36, 14) "Coordinates" TypeName]
    )

test_parseTopDecl_Data :: Test
test_parseTopDecl_Data =
  TestCase
    ( canParse
        parseTopDecl
        [si (0, 0) "data" (Keyword Data), si (0, 0) "KeywordToken" TypeName, si (0, 0) "=" Equals, si (0, 0) "Module" TypeName, si (0, 0) "|" Pipe, si (0, 0) "Where" TypeName, si (0, 0) "|" Pipe, si (0, 0) "Data" TypeName, si (0, 0) "deriving" (Keyword Tokens.Deriving), si (0, 0) "(" LeftParan, si (0, 0) "Eq" TypeName, si (0, 0) "," Comma, si (0, 0) "Ord" TypeName, si (0, 0) "," Comma, si (0, 0) "Show" TypeName, si (0, 0) ")" RightParan]
    )

test_parseTopDecl_Instance :: Test
test_parseTopDecl_Instance =
  TestCase
    ( canParse
        parseTopDecl
        [si (3, 1) "instance" (Keyword Instance), si (3, 10) "Functor" TypeName, si (3, 18) "ScanItem" TypeName, si (3, 27) "where" (Keyword Where), si (0, 0) "{" LeftBrace, si (4, 3) "fmap" ValueName, si (4, 8) "fab" ValueName, si (4, 12) "sia" ValueName, si (4, 16) "=" Equals, si (5, 5) "ScanItem" TypeName, si (0, 0) "{" LeftBrace, si (6, 9) "scanLoc" ValueName, si (6, 17) "=" Equals, si (6, 19) "scanLoc" ValueName, si (6, 27) "sia" ValueName, si (6, 30) "," Comma, si (7, 9) "scanStr" ValueName, si (7, 17) "=" Equals, si (7, 19) "scanStr" ValueName, si (7, 27) "sia" ValueName, si (7, 30) "," Comma, si (8, 9) "scanTok" ValueName, si (8, 17) "=" Equals, si (8, 19) "fab" ValueName, si (8, 23) "$" Varsym, si (8, 25) "scanTok" ValueName, si (8, 33) "sia" ValueName, si (0, 0) "}" RightBrace, si (0, 0) "}" RightBrace]
    )

test_parseDecls :: Test
test_parseDecls =
  TestCase
    ( canParse
        parseDecls
        [si (0, 0) "{" LeftBrace, si (16, 5) "x" ValueName, si (16, 7) "=" Equals, si (16, 9) "1" IntegerLiteral, si (0, 0) ";" SemiColon, si (17, 5) "y" ValueName, si (17, 7) "=" Equals, si (17, 9) "2" IntegerLiteral, si (0, 0) "}" RightBrace]
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

test_parseIDecl :: Test
test_parseIDecl =
  TestCase
    ( canParse
        parseIDecl
        [si (4, 3) "fmap" ValueName, si (4, 8) "fab" ValueName, si (4, 12) "sia" ValueName, si (4, 16) "=" Equals, si (5, 5) "ScanItem" TypeName, si (0, 0) "{" LeftBrace, si (6, 9) "scanLoc" ValueName, si (6, 17) "=" Equals, si (6, 19) "scanLoc" ValueName, si (6, 27) "sia" ValueName, si (6, 30) "," Comma, si (7, 9) "scanStr" ValueName, si (7, 17) "=" Equals, si (7, 19) "scanStr" ValueName, si (7, 27) "sia" ValueName, si (7, 30) "," Comma, si (8, 9) "scanTok" ValueName, si (8, 17) "=" Equals, si (8, 19) "fab" ValueName, si (8, 23) "$" Varsym, si (8, 25) "scanTok" ValueName, si (8, 33) "sia" ValueName, si (0, 0) "}" RightBrace]
    )

test_parseConstrs :: Test
test_parseConstrs =
  TestCase
    ( canParse
        parseConstrs
        [si (0, 0) "Module" TypeName, si (0, 0) "|" Pipe, si (0, 0) "Where" TypeName, si (0, 0) "|" Pipe, si (0, 0) "Data" TypeName]
    )

test_parseConstr_TypeName :: Test
test_parseConstr_TypeName =
  TestCase
    ( canParse
        parseConstr
        [si (0, 0) "Module" TypeName]
    )

test_parseConstr_FieldDecls :: Test
test_parseConstr_FieldDecls =
  TestCase
    ( canParse
        parseConstr
        [si (3, 19) "ScanItem" TypeName, si (0, 0) "{" LeftBrace, si (4, 5) "scanLoc" ValueName, si (4, 13) "::" DoubleColon, si (4, 16) "Coordinates" TypeName, si (4, 27) "," Comma, si (5, 5) "scanStr" ValueName, si (5, 13) "::" DoubleColon, si (5, 16) "String" TypeName, si (5, 22) "," Comma, si (6, 5) "scanTok" ValueName, si (6, 13) "::" DoubleColon, si (6, 16) "a" ValueName, si (0, 0) "}" RightBrace]
    )

test_parseFieldDecl :: Test
test_parseFieldDecl =
  TestCase
    ( canParse
        parseFieldDecl
        [si (4, 5) "scanLoc" ValueName, si (4, 13) "::" DoubleColon, si (4, 16) "Coordinates" TypeName]
    )

test_parseDeriving :: Test
test_parseDeriving =
  TestCase
    ( canParse
        parseDeriving
        [si (0, 0) "deriving" (Keyword Tokens.Deriving), si (0, 0) "(" LeftParan, si (0, 0) "Eq" TypeName, si (0, 0) "," Comma, si (0, 0) "Ord" TypeName, si (0, 0) "," Comma, si (0, 0) "Show" TypeName, si (0, 0) ")" RightParan]
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

test_parseGuard :: Test
test_parseGuard =
  TestCase
    ( canParse
        parseGuard
        [si (21, 5) "scanTok" ValueName, si (21, 14) "==" Varsym, si (21, 16) "1" IntegerLiteral]
    )

test_parseGuard_fail_ReservedOp :: Test
test_parseGuard_fail_ReservedOp =
  TestCase
    ( cannotParse
        parseGuard
        [si (21, 5) "scanTok" ValueName, si (21, 14) "=" Equals, si (21, 16) "1" IntegerLiteral]
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
        [si (20, 47) "ab" ValueName, si (20, 50) "==" Varsym, si (20, 53) "2" IntegerLiteral]
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

test_parseAexp_LabelCon :: Test
test_parseAexp_LabelCon =
  TestCase
    ( canParse
        parseAExp
        [si (5, 5) "ScanItem" TypeName, si (0, 0) "{" LeftBrace, si (6, 9) "scanLoc" ValueName, si (6, 17) "=" Equals, si (6, 19) "scanLoc" ValueName, si (6, 27) "sia" ValueName, si (6, 30) "," Comma, si (7, 9) "scanStr" ValueName, si (7, 17) "=" Equals, si (7, 19) "scanStr" ValueName, si (7, 27) "sia" ValueName, si (7, 30) "," Comma, si (8, 9) "scanTok" ValueName, si (8, 17) "=" Equals, si (8, 19) "fab" ValueName, si (8, 23) "$" Varsym, si (8, 25) "scanTok" ValueName, si (8, 33) "sia" ValueName, si (0, 0) "}" RightBrace]
    )

test_parseStmts :: Test
test_parseStmts =
  TestCase
    ( canParse
        parseStmts
        [si (7, 3) "x" ValueName, si (7, 5) "<-" LeftArrow, si (7, 8) "pa" ValueName, si (7, 9) ";" SemiColon, si (7, 3) "return" ValueName, si (7, 10) "pa" ValueName]
    )

test_parseStmt :: Test
test_parseStmt =
  TestCase
    ( canParse
        parseStmt
        [si (7, 3) "x" ValueName, si (7, 5) "<-" LeftArrow, si (7, 8) "pa" ValueName, si (7, 9) ";" SemiColon]
    )

test_parseFBind :: Test
test_parseFBind =
  TestCase
    ( canParse
        parseFBind
        [si (8, 9) "scanTok" ValueName, si (8, 17) "=" Equals, si (8, 19) "fab" ValueName, si (8, 23) "$" Varsym, si (8, 25) "scanTok" ValueName, si (8, 33) "sia" ValueName]
    )

test_parseFBind_fail_RightBrace :: Test
test_parseFBind_fail_RightBrace =
  TestCase
    ( cannotParse
        parseFBind
        [si (8, 9) "scanTok" ValueName, si (8, 17) "=" Equals, si (8, 19) "fab" ValueName, si (8, 23) "$" Varsym, si (8, 25) "scanTok" ValueName, si (8, 33) "sia" ValueName, si (8, 36) "}" RightBrace]
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
        [si (72, 23) "Si" TypeName, si (72, 26) "a" ValueName]
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
        [si (72, 23) "Si" TypeName]
    )

test_parseVarOp :: Test
test_parseVarOp =
  TestCase
    ( canParse
        parseVarOp
        [si (0, 0) "++" Varsym]
    )

test_parseQOp :: Test
test_parseQOp =
  TestCase
    ( canParse
        parseQOp
        [si (0, 0) "==" Varsym]
    )

test_parseQOp_fail_reservedOp :: Test
test_parseQOp_fail_reservedOp =
  TestCase
    ( cannotParse
        parseQOp
        [si (0, 0) "=" Equals]
    )

parserTests :: Test
parserTests =
  TestLabel
    "ParserTests"
    ( TestList
        [ TestLabel "test_parseVarSym" test_parseVarSym,
          TestLabel "test_parseConSym" test_parseConSym,
          TestLabel "test_parseQTyCon" test_parseQTyCon,
          TestLabel "test_parseTopDecl_Data" test_parseTopDecl_Data,
          TestLabel "test_parseTopDecl_Instance" test_parseTopDecl_Instance,
          TestLabel "test_parseDecls" test_parseDecls,
          TestLabel "test_parseDecl_GenDecl" test_parseDecl_GenDecl,
          TestLabel "test_parseDecl_Pat" test_parseDecl_Pat,
          TestLabel "test_parseDecl_FunLhs" test_parseDecl_FunLhs,
          TestLabel "test_parseIDecl" test_parseIDecl,
          TestLabel "test_parseConstrs" test_parseConstrs,
          TestLabel "test_parseConstr_TypeName" test_parseConstr_TypeName,
          TestLabel "test_parseConstr_FieldDecls" test_parseConstr_FieldDecls,
          TestLabel "test_parseFieldDecl" test_parseFieldDecl,
          TestLabel "test_parseDeriving" test_parseDeriving,
          TestLabel "test_parseFunLhs_Var" test_parseFunLhs_Var,
          TestLabel "test_parseFunLhs_Pat" test_parseFunLhs_Pat,
          TestLabel "test_parseRhs_Exp1" test_parseRhs_Exp1,
          TestLabel "test_parseRhs_Exp2" test_parseRhs_Exp2,
          TestLabel "test_parseGdRhs" test_parseGdRhs,
          TestLabel "test_parseGuards" test_parseGuards,
          TestLabel "test_parseGuard" test_parseGuard,
          TestLabel "test_parseGuard_fail_ReservedOp" test_parseGuard_fail_ReservedOp,
          TestLabel "test_parseExp_Literal" test_parseExp_Literal,
          TestLabel "test_parseExp_DoubleEquals" test_parseExp_DoubleEquals,
          TestLabel "test_parseLExp" test_parseLExp,
          TestLabel "test_parseInfixExp" test_parseInfixExp,
          TestLabel "test_parseAexp_LabelCon" test_parseAexp_LabelCon,
          TestLabel "test_parseStmts" test_parseStmts,
          TestLabel "test_parseStmt" test_parseStmt,
          TestLabel "test_parseFBind" test_parseFBind,
          TestLabel "test_parseFBind_fail_RightBrace" test_parseFBind_fail_RightBrace,
          TestLabel "test_parsePat_LPat" test_parsePat_LPat,
          TestLabel "test_parseLPat_GCon" test_parseLPat_GCon,
          TestLabel "test_parseAPat_Var" test_parseAPat_Var,
          TestLabel "test_parseAPat_head_tail" test_parseAPat_head_tail,
          TestLabel "test_parseGCon_QCon" test_parseGCon_QCon,
          TestLabel "test_parseVarOp" test_parseVarOp,
          TestLabel "test_parseQOp" test_parseQOp,
          TestLabel "test_parseQOp_fail_reservedOp" test_parseQOp_fail_reservedOp
        ]
    )
