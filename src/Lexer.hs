{-# LANGUAGE TupleSections #-}

module Lexer
  ( lexer,
    ScanItem (..),
  )
where

import Common (Coordinates)
import Data.Char
  ( isAlpha,
    isAlphaNum,
    isLower,
    isNumber,
    isUpper,
  )
import Data.Maybe
  ( fromJust,
    isJust,
    listToMaybe,
  )
import Tokens

{- Wrap scanned tokens in ScanItem to add information about context -}
data ScanItem a = ScanItem
  { scanLoc :: Coordinates,
    scanStr :: String,
    scanTok :: a
  }
  deriving (Eq)

instance Functor ScanItem where
  fmap fab sia =
    ScanItem
      { scanLoc = scanLoc sia,
        scanStr = scanStr sia,
        scanTok = fab $ scanTok sia
      }

instance Applicative ScanItem where
  pure a = ScanItem (1, 1) "" a
  siab <*> sia =
    ScanItem
      { scanLoc = scanLoc siab,
        scanStr = scanStr siab ++ scanStr sia,
        scanTok = scanTok siab $ scanTok sia
      }

instance Show a => Show (ScanItem a) where
  show (ScanItem coo str tok) =
    mconcat
      [ "{ ",
        show coo,
        " ",
        show str,
        " ",
        show tok,
        " }"
      ]

{- The scanner takes as an input a tuple of current scan location
   & the part of the string not yet scanned -}
type Input = (Coordinates, String)

-- Turns the input into a stream of scanned tokens
lexer :: String -> [ScanItem Token]
lexer input = scan [] ((1, 1), input)

-- Recursive scan until whole input has been consumed
scan :: [ScanItem Token] -> Input -> [ScanItem Token]
scan prevTokens ((r, c), remInput) =
  if tok == EOF
    then currTokens
    else scan currTokens (nextCoords, nextInput)
  where
    (tok, str) = getNextToken remInput
    currTokens = prevTokens ++ [ScanItem (r, c) str tok]
    nextInput = drop (length str) remInput
    nextCoords =
      if tok == NewLine || tok == LineComment
        then (r + 1, 1)
        else (r, c + length str)

-- Matcher state
data State
  = Start -- matcher started
  | Middle -- saw starting character, e.g. double quotes for string literal
  | End Token -- saw ending character and output token
  | Escape -- previous character was escape
  deriving (Eq, Ord, Show)

-- current state -> next char -> next index -> new state or Nothing if no match
type Matcher = State -> Char -> Int -> Maybe State

-- TODO handle escaping
matchStringLiteral :: State -> Char -> Int -> Maybe State
matchStringLiteral Start '"' 0 = Just Middle
matchStringLiteral Middle '"' _ = Just (End StringLiteral)
matchStringLiteral Middle _ _ = Just Middle
matchStringLiteral _ _ _ = Nothing

matchCharLiteral :: State -> Char -> Int -> Maybe State
matchCharLiteral Start '\'' 0 = Just Middle
matchCharLiteral Middle '\'' _ = Just (End StringLiteral)
matchCharLiteral Middle _ _ = Just Middle
matchCharLiteral _ _ _ = Nothing

matchNumberLiteral :: State -> Char -> Int -> Maybe State
matchNumberLiteral Start c 0 =
  if isNumber c
    then Just (End IntegerLiteral)
    else Nothing
matchNumberLiteral (End IntegerLiteral) c _ =
  if isNumber c || c == '.'
    then Just (End IntegerLiteral)
    else Nothing
matchNumberLiteral _ _ _ = Nothing

matchTypeName :: State -> Char -> Int -> Maybe State
matchTypeName Start c 0 =
  if isAlpha c && isUpper c
    then Just (End TypeName)
    else Nothing
matchTypeName (End TypeName) c _ =
  if isAlphaNum c || isUnderscore c || isApostrophe c
    then Just (End TypeName)
    else Nothing
matchTypeName _ _ _ = Nothing

matchValName :: State -> Char -> Int -> Maybe State
matchValName Start c 0 =
  if (isAlpha c && isLower c) || isUnderscore c
    then Just (End ValueName)
    else Nothing
matchValName (End ValueName) c _ =
  if isAlphaNum c || isUnderscore c || isApostrophe c
    then Just (End ValueName)
    else Nothing
matchValName _ _ _ = Nothing

matchLineComment :: State -> Char -> Int -> Maybe State
matchLineComment Start c 0
  | c == '-' = Just Start
matchLineComment Start c 1
  | c == '-' = Just Middle
matchLineComment Middle c n
  | c /= '\n' = Just Middle
matchLineComment Middle c n
  | c == '\n' = Just (End LineComment)
matchLineComment _ _ _ = Nothing

isUnderscore :: Char -> Bool
isUnderscore c = c == '_'

isApostrophe :: Char -> Bool
isApostrophe c = c == '\''

-- Matches any single character word
matchOther :: State -> Char -> Int -> Maybe State
matchOther _ _ 0 = Just (End Other)
matchOther _ _ _ = Nothing

-- Returns a matcher that matches only the given word exactly
generateMatcher :: String -> Token -> Matcher
generateMatcher word token = matcher
  where
    matcher s c i
      | i >= length word = Nothing
      | c == word !! i =
        if i + 1 == length word
          then Just (End token)
          else Just Middle
      | otherwise = Nothing

-- The list of matchers that look for an exact keyword
keywordMatchers :: [Matcher]
keywordMatchers = map (uncurry fn) keywords
  where
    fn word keywordToken = generateMatcher word (Keyword keywordToken)
    keywords =
      [ ("where", Tokens.Where),
        ("module", Tokens.Module),
        ("data", Tokens.Data),
        ("deriving", Tokens.Deriving),
        ("instance", Tokens.Instance),
        ("type", Tokens.Type),
        ("newtype", Tokens.NewType),
        ("class", Tokens.Class),
        ("default", Tokens.Default),
        ("let", Tokens.Let),
        ("in", Tokens.In),
        ("of", Tokens.Of),
        ("do", Tokens.Do),
        ("deriving", Tokens.Deriving)
      ]

allMatchers :: [Matcher]
allMatchers =
  keywordMatchers
    -- Names
    ++ [ matchTypeName,
         matchValName
       ]
    -- Literals
    ++ [ matchStringLiteral,
         matchCharLiteral,
         matchNumberLiteral,
         matchLineComment
       ]
    -- Symbols
    ++ [ generateMatcher "=" Equals,
         generateMatcher "->" SingleArrow,
         generateMatcher "=>" DoubleArrow,
         generateMatcher ":" Colon,
         generateMatcher "::" DoubleColon,
         generateMatcher ".." DoubleDot,
         generateMatcher " " Space,
         generateMatcher "\n" NewLine,
         generateMatcher "!" Exclamation,
         generateMatcher "#" Hash,
         generateMatcher "$" Dollar,
         generateMatcher "%" Percent,
         generateMatcher "&" Ampersand,
         generateMatcher "*" Asterisk,
         generateMatcher "+" Plus,
         generateMatcher "." Dot,
         generateMatcher "/" Divide,
         generateMatcher "<" LeftAngle,
         generateMatcher "=" Equals,
         generateMatcher ">" RightAngle,
         generateMatcher "?" Question,
         generateMatcher "@" At,
         generateMatcher "\\" Backslash,
         generateMatcher "^" Caret,
         generateMatcher "|" Pipe,
         generateMatcher "-" Dash,
         generateMatcher "~" Tilde,
         generateMatcher ";" SemiColon,
         generateMatcher "," Comma,
         generateMatcher "`" BackTick,
         generateMatcher "(" LeftParan,
         generateMatcher ")" RightParan,
         generateMatcher "[" LeftBracket,
         generateMatcher "]" RightBracket,
         generateMatcher "{" LeftBrace,
         generateMatcher "}" RightBrace
       ]
    ++ [matchOther] -- has to be last matcher, because it matches on everything

-- Get next token from the input
getNextToken :: String -> (Token, String)
getNextToken input = findLongestMatch input 0 initMatchers (EOF, "")
  where
    initMatchers = map (Start,) allMatchers

-- Keeps track of the input, the position in the input, the matcher states and the longest match until now
findLongestMatch :: String -> Int -> [(State, Matcher)] -> (Token, String) -> (Token, String)
findLongestMatch input idx prevMatchers prevLongestMatch
  | idx >= length input = prevLongestMatch
  | (not . null) currMatchers = findLongestMatch input (idx + 1) currMatchers currLongestMatch
  --  | (not . null) currMatchers = traceShow (input, idx, map fst prevMatchers, prevLongestMatch) (findLongestMatch input (idx + 1) currMatchers currLongestMatch)
  | otherwise = prevLongestMatch
  where
    currMatchers = updateMatchers (input !! idx) idx prevMatchers
    firstMatch = (listToMaybe . map fst . filter (\(s, m) -> isEnd s)) currMatchers
    firstMatchToken = firstMatch >>= getToken
    currLongestMatch =
      if isJust firstMatchToken
        then (fromJust firstMatchToken, take (idx + 1) input)
        else prevLongestMatch

-- Returns the token if the matcher state is End, else Nothing
getToken :: State -> Maybe Token
getToken (End t) = Just t
getToken _ = Nothing

-- Returns whether the state is End
isEnd :: State -> Bool
isEnd (End _) = True
isEnd _ = False

-- Updates matchers on next character. Then removes matchers without a (partial) match
updateMatchers :: Char -> Int -> [(State, Matcher)] -> [(State, Matcher)]
updateMatchers char idx = map getMatch . filter isMatch . map runMatcher
  where
    runMatcher (state, matcher) = (matcher state char idx, matcher)
    isMatch (maybeState, matcher) = isJust maybeState
    getMatch (maybeState, matcher) = (fromJust maybeState, matcher)
