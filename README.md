# haskell-parser

This is my attempt to create a Haskell parser that is written in Haskell. The goal was to create a parser that can parse
its own code.

It is based on the [Haskell2010 syntax](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17500010).

**This is by no means production-ready. A part of the Haskell syntax has not been implemented. Also, there will probably
be bugs and suboptimal code. No guarantees of correctness are given.**

## How it works

Overall, the concept of a parser is simple: it turns raw code into an abstract syntax tree. This syntax tree could be
potentially optimized afterward. These are often the first steps of an interpreter or compiler. The next step could be
executing the code or translating it to another programming language.

My implementation of a parser consists of 4 steps.

### 1. Prescan

This takes the raw Haskell code of a single module as input. Then it looks for pragma definitions at the start of the
string. Each line containing a pragma is replaced by an empty line. The processed lines and the separate pragmas are
returned.

The pragmas are not utilized at the moment (though they could be in a more advanced parser implementation). Only the
prescanned code feeds into the next step.

### 2. Lexer

This takes the prescanned code as input and converts it to a list of tokens. A token is the smallest parsable unit of
the syntax. All the possible tokens are defined in `Tokens.hs`.

The algorithm for the lexer is somewhat home-grown. It scans the input for the longest string that matches a token. Once
it finds that longest substring, it pops it off together with a token and the string location. After that, it looks for
the next token.

Scanning for the longest substring is not an uncommon idea. But the approach in which the strings are matched might be.
For each token, there is a state machine that keeps track of whether the scanned letters are allowed in the current
position for the token.

### 3. Layout

Haskell is normally layout-sensitive. This means that the way that the code is laid out can change the semantic meaning.
Therefore, an intermediate step is needed to make the stream of tokens layout-insensitive. This requires inserting
additional tokens like `{`, `}` and `;` in specific places.

The algorithm for doing this is provided in section 10.3 of
the [Haskell2010 syntax](https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17500010).

### 4. Parser

The final step is to convert the stream of layout-insensitive tokens to an abstract syntax tree. All the possible
elements of the tree are defined in `Elements.hs`. The parser combinators can be found in `ParserHelpers.hs`. The method
of parsing is [recursive descent](https://en.wikipedia.org/wiki/Recursive_descent_parser).

### In summary...

![Parser](https://user-images.githubusercontent.com/6129322/139604971-a582b045-a0f4-4cc8-9b29-5dd9a0b6cf21.png)

### Example

Now I provide the input, output, and intermediate products for the parsing of a seemingly simple program. The program
calculates the Fibonacci sequence. It was taken from [here](https://wiki.haskell.org/The_Fibonacci_sequence).

Input:

```haskell
{-# LANGUAGE BangPatterns #-}

module Fibonacci where

fibs = 0 : 1 : next fibs
  where
    next (a : t@(b:_)) = (a+b) : next t
```

**1. Prescan**

Prescanned code:

```haskell
module Fibonacci where

fibs = 0 : 1 : next fibs
  where
    next (a : t@(b:_)) = (a+b) : next t
```

Pragmas:

```haskell
[Language "BangPatterns"]
```

**2. Lexer**

Tokens:
(omitted for brevity)

**3. Layout**

Layout-insensitive-tokens:

```haskell
[ScanItem (3,1) "module" Keyword Module,ScanItem (3,8) "Sample" TypeName,ScanItem (3,15) "where" Keyword Where,ScanItem (0,0) "{" LeftBrace,ScanItem (5,1) "fibs" ValueName,ScanItem (5,6) "=" Equals,ScanItem (5,8) "0" IntegerLiteral,ScanItem (5,10) ":" Colon,ScanItem (5,12) "1" IntegerLiteral,ScanItem (5,14) ":" Colon,ScanItem (5,16) "next" ValueName,ScanItem (5,21) "fibs" ValueName,ScanItem (6,3) "where" Keyword Where,ScanItem (0,0) "{" LeftBrace,ScanItem (7,5) "next" ValueName,ScanItem (7,10) "(" LeftParan,ScanItem (7,11) "a" ValueName,ScanItem (7,13) ":" Colon,ScanItem (7,15) "t" ValueName,ScanItem (7,16) "@" At,ScanItem (7,17) "(" LeftParan,ScanItem (7,18) "b" ValueName,ScanItem (7,19) ":" Colon,ScanItem (7,20) "_" ValueName,ScanItem (7,21) ")" RightParan,ScanItem (7,22) ")" RightParan,ScanItem (7,24) "=" Equals,ScanItem (7,26) "(" LeftParan,ScanItem (7,27) "a" ValueName,ScanItem (7,28) "+" Varsym,ScanItem (7,29) "b" ValueName,ScanItem (7,30) ")" RightParan,ScanItem (7,32) ":" Colon,ScanItem (7,34) "next" ValueName,ScanItem (7,39) "t" ValueName,ScanItem (0,0) "}" RightBrace,ScanItem (0,0) "}" RightBrace]
```

**4. Parser**

Abstract syntax tree:

```haskell
Module
  (ModId "Sample") Nothing
  (Body
    [
    ]
    [
      TopDecl_Decl
        (Decl_Pat
          (Pat_LPat
            (LPat_APat
              (APat_Var
                (Var_VarId
                  (VarId "fibs")) Nothing)))
          (Rhs_Exp
            (Exp_InfixExp
              (InfixExp_Infix
                (LExp_FExp
                  (FExp
                    (AExp_Lit
                      (Literal_Int
                        (LitInteger "0")))
                    [
                    ]
                  ))
                (QOp_QConOp
                  (QConOp_GConSym GConSym_Colon))
                (InfixExp_Infix
                  (LExp_FExp
                    (FExp
                      (AExp_Lit
                        (Literal_Int
                          (LitInteger "1")))
                      [
                      ]
                    ))
                  (QOp_QConOp
                    (QConOp_GConSym GConSym_Colon))
                  (InfixExp_LExp
                    (LExp_FExp
                      (FExp
                        (AExp_QVar
                          (QVar_QVarId
                            (QVarId Nothing
                              (VarId "next"))))
                        [
                          AExp_QVar
                            (QVar_QVarId
                              (QVarId Nothing
                                (VarId "fibs")))
                        ]
                      ))))))
            (Just
              [
                Decl_FunLhs
                  (FunLhs_Var
                    (Var_VarId
                      (VarId "next"))
                    [
                      APat_Paran
                        (Pat_Infix
                          (LPat_APat
                            (APat_Var
                              (Var_VarId
                                (VarId "a")) Nothing))
                          (QConOp_GConSym GConSym_Colon)
                          (Pat_LPat
                            (LPat_APat
                              (APat_Var
                                (Var_VarId
                                  (VarId "t"))
                                (Just
                                  (APat_Paran
                                    (Pat_Infix
                                      (LPat_APat
                                        (APat_Var
                                          (Var_VarId
                                            (VarId "b")) Nothing))
                                      (QConOp_GConSym GConSym_Colon)
                                      (Pat_LPat
                                        (LPat_APat
                                          (APat_Var
                                            (Var_VarId
                                              (VarId "_")) Nothing))))))))))
                    ]
                  )
                  (Rhs_Exp
                    (Exp_InfixExp
                      (InfixExp_Infix
                        (LExp_FExp
                          (FExp
                            (AExp_ParanExp
                              (Exp_InfixExp
                                (InfixExp_Infix
                                  (LExp_FExp
                                    (FExp
                                      (AExp_QVar
                                        (QVar_QVarId
                                          (QVarId Nothing
                                            (VarId "a"))))
                                      [
                                      ]
                                    ))
                                  (QOp_QVarOp
                                    (QVarOp_QVarSym
                                      (QVarSym Nothing
                                        (VarSym "+"))))
                                  (InfixExp_LExp
                                    (LExp_FExp
                                      (FExp
                                        (AExp_QVar
                                          (QVar_QVarId
                                            (QVarId Nothing
                                              (VarId "b"))))
                                        [
                                        ]
                                      ))))))
                            [
                            ]
                          ))
                        (QOp_QConOp
                          (QConOp_GConSym GConSym_Colon))
                        (InfixExp_LExp
                          (LExp_FExp
                            (FExp
                              (AExp_QVar
                                (QVar_QVarId
                                  (QVarId Nothing
                                    (VarId "next"))))
                              [
                                AExp_QVar
                                  (QVar_QVarId
                                    (QVarId Nothing
                                      (VarId "t")))
                              ]
                            ))))) Nothing)
              ]
            )))
    ]
  )
```

## Credits

Lots of inspiration was taken from https://github.com/jrauhamaa/hc.
