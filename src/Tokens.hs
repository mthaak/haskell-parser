module Tokens
  ( KeywordToken (..),
    Token (..),
    isReservedOp,
    isDashes,
    isSymbol,
    ascSymbolTokens,
  )
where

data KeywordToken
  = -- reservedid:
    Case -- case
  | Class -- class
  | Data -- data
  | Default -- default
  | Deriving -- deriving
  | Do -- do
  | Else -- else
  | Foreign -- foreign
  | If -- If
  | Import -- import
  | In -- in
  | Infix -- infix
  | Infixl -- infixl
  | Infixr -- infixr
  | Instance -- instance
  | Let -- let
  | Module -- module
  | NewType -- newtype
  | Of -- of
  | Then -- then
  | Type -- type
  | Where -- where
  deriving (Eq, Ord, Show)

-- TODO blockcomment
-- TODO tab
data Token
  = Keyword KeywordToken -- any special keyword
  | -- Names
    TypeName -- Type
  | ValueName -- val
  | Varsym -- ++
  | Consym -- :+
  | -- Literals
    IntegerLiteral -- 12
  | FloatLiteral -- 12.3
  | StringLiteral -- "abc"
  | CharLiteral -- 'a'
  | LineComment -- --abc
  | -- Symbols
    SingleArrow -- ->
  | DoubleRightArrow -- =>
  | LeftArrow -- <-
  | DoubleColon -- ::
  | DoubleDot -- ..
  | Space -- " "
  | NewLine -- \n
  | Exclamation -- !
  | Hash -- #
  | Dollar -- \$
  | Percent -- %
  | Ampersand -- &
  | Asterisk -- \*
  | Plus -- +
  | Dot -- .
  | Divide -- /
  | LeftAngle -- <
  | Equals -- =
  | RightAngle -- >
  | Question -- ?
  | At -- @
  | Backslash -- \
  | Caret -- \^
  | Pipe -- \|
  | Dash -- -
  | Tilde -- ~
  | Colon -- :
  | SemiColon -- ;
  | Comma -- ,
  | BackTick -- `
  | LeftParan -- (
  | RightParan -- )
  | LeftBracket -- [
  | RightBracket -- ]
  | LeftBrace -- {
  | RightBrace -- }
  | Underscore -- _
  | EOF -- end of file
  | Other -- anything else
  deriving (Eq, Ord, Show)

reservedOpList :: [String]
reservedOpList =
  [ "..",
    ":",
    "::",
    "=",
    "\\",
    "|",
    "<-",
    "->",
    "@",
    "~",
    "=>"
  ]

ascSymbolStrings :: [Char]
ascSymbolStrings =
  [ '!',
    '#',
    '$',
    '%',
    '&',
    '*',
    '+',
    '.',
    '/',
    '<',
    '=',
    '>',
    '?',
    '@',
    '\\',
    '^',
    '|',
    '-',
    '~',
    ':'
  ]

ascSymbolTokens :: [Token]
ascSymbolTokens =
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

isReservedOp :: String -> Bool
isReservedOp str = str `elem` reservedOpList

isDashes :: String -> Bool
isDashes str = length str >= 2 && all (== '-') str

isAscSymbol :: Char -> Bool
isAscSymbol str = str `elem` ascSymbolStrings

isSymbol :: Char -> Bool
isSymbol = isAscSymbol -- TODO add uniSymbol
