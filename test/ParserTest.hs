module ParserTest
  ( parserTests,
  )
where

import Parser
import Test.HUnit
import Tokens

canParse :: Parser a -> Input -> Assertion
canParse pa input = case runParser pa input of
  Right (_, []) -> assertBool "" True
  Right (_, _) -> assertBool "" False
  Left _ -> assertBool "" False

test_parseDecl_FunLhs :: Test
test_parseDecl_FunLhs =
  TestCase
    ( canParse
        parseDecl
        [(ValueName, "add"), (ValueName, "x"), (Equals, "="), (ValueName, "x"), (Plus, "+"), (IntegerLiteral, "3")]
    )

test_parseDecl_GenDecl :: Test
test_parseDecl_GenDecl =
  TestCase
    ( canParse
        parseDecl
        [(ValueName, "add"), (DoubleColon, "::"), (TypeName, "Num"), (ValueName, "a"), (DoubleArrow, "=>"), (ValueName, "a"), (SingleArrow, "->"), (ValueName, "a")]
    )

test_parseFunLhs_Var :: Test
test_parseFunLhs_Var =
  TestCase
    ( canParse
        parseFunLhs
        [(ValueName, "add"), (ValueName, "x")]
    )

test_parseRhs_Exp :: Test
test_parseRhs_Exp =
  TestCase
    ( canParse
        parseRhs
        [(Equals, "="), (ValueName, "x"), (Plus, "+"), (IntegerLiteral, "3")]
    )

test_parseVarSym :: Test
test_parseVarSym =
  TestCase
    ( canParse
        parseVarSym
        [(LeftAngle, "<"), (Dollar, "$"), (RightAngle, ">")]
    )

test_parseLExp :: Test
test_parseLExp =
  TestCase
    ( canParse
        parseLExp
        [(LeftParan, "("), (Plus, "+"), (IntegerLiteral, "1"), (RightParan, ")")]
    )

test_parseInfixExp :: Test
test_parseInfixExp =
  TestCase
    ( canParse
        parseInfixExp
        [(TypeName, "Just"), (IntegerLiteral, "1")]
    )

test_ParseConstrs :: Test
test_ParseConstrs =
  TestCase
    ( canParse
        parseConstrs
        [(TypeName, "Module"), (Pipe, "|"), (TypeName, "Where"), (Pipe, "|"), (TypeName, "Data")]
    )

test_ParseConstr :: Test
test_ParseConstr =
  TestCase
    ( canParse
        parseConstr
        [(TypeName, "Module")]
    )

test_ParseTopDecl_Data :: Test
test_ParseTopDecl_Data =
  TestCase
    ( canParse
        parseTopDecl
        [(Keyword Data, "data"), (TypeName, "KeywordToken"), (Equals, "="), (TypeName, "Module"), (Pipe, "|"), (TypeName, "Where"), (Pipe, "|"), (TypeName, "Data"), (Keyword Tokens.Deriving, "deriving"), (LeftParan, "("), (TypeName, "Eq"), (Comma, ","), (TypeName, "Ord"), (Comma, ","), (TypeName, "Show"), (RightParan, ")")]
    )

test_parseDeriving :: Test
test_parseDeriving =
  TestCase
    ( canParse
        parseDeriving
        [(Keyword Tokens.Deriving, "deriving"), (LeftParan, "("), (TypeName, "Eq"), (Comma, ","), (TypeName, "Ord"), (Comma, ","), (TypeName, "Show"), (RightParan, ")")]
    )

parserTests :: Test
parserTests =
  TestLabel
    "ParserTests"
    ( TestList
        [ TestLabel "test_parseDecl_GenDecl" test_parseDecl_GenDecl,
          TestLabel "test_parseDecl_FunLhs" test_parseDecl_FunLhs,
          TestLabel "test_parseFunLhs_Var" test_parseFunLhs_Var,
          TestLabel "test_parseRhs_Exp" test_parseRhs_Exp,
          TestLabel "test_parseVarSym" test_parseVarSym,
          TestLabel "test_parseLExp" test_parseLExp,
          TestLabel "test_parseInfixExp" test_parseInfixExp,
          TestLabel "test_ParseTopDecl_Data" test_ParseTopDecl_Data,
          TestLabel "test_ParseConstrs" test_ParseConstrs,
          TestLabel "test_ParseConstr" test_ParseConstr,
          TestLabel "test_parseDeriving" test_parseDeriving
        ]
    )
