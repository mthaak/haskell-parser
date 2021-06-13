module Tokens
  ( KeywordToken (..),
    Token (..),
  )
where

-- TODO more keywords
data KeywordToken
  = Module -- module
  | Where -- where
  | Data -- data
  | Deriving -- deriving
  | Instance -- instance
  | Type -- type
  | NewType -- newtype
  | Class -- class
  | Default -- default
  | Let -- let
  | In -- in
  | Of -- of
  | Do -- do
  deriving (Eq, Ord, Show)

-- TODO blockcomment
-- TODO tab
data Token
  = Keyword KeywordToken -- any special keyword
  | -- Names
    TypeName -- Type
  | ValueName -- val
  | -- Literals
    IntegerLiteral -- 12
  | FloatLiteral -- 12.3
  | StringLiteral -- "abc"
  | CharLiteral -- 'a'
  | LineComment -- --abc
  | -- Symbols
    SingleArrow -- ->
  | DoubleArrow -- =>
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
  | EOF -- end of file
  | Other -- anything else
  deriving (Eq, Ord, Show)
