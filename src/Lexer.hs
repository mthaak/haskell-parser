{-# LANGUAGE TupleSections #-}

module Lexer
  ( lexer,
    ScanItem (..),
    si,
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
import Data.Either
  ( rights,
  )
import Data.Maybe
  ( fromJust,
    isJust,
  )
import Debug.Trace (traceShowId)
import Tokens
import Utils

{- Wrap scanned tokens in ScanItem to add information about context -}
data ScanItem a = ScanItem
  { scanLoc :: Coordinates,
    scanStr :: String,
    scanTok :: a
  }
  deriving (Eq)

si :: Common.Coordinates -> String -> a -> ScanItem a
si = ScanItem

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
      [ "ScanItem ",
        show coo,
        " ",
        show str,
        " ",
        show tok
      ]

data ScanError = ScanError
  deriving (Eq, Show)

-- Type of each scanner
newtype Scanner = Scanner
  { runScanner :: Input -> Either ScanError (Input, ScanItem Token)
  }

{- The scanner takes as an input a tuple of current scan location
   & the part of the string not yet scanned -}
type Input = (Coordinates, String)

-- Turns the input into a stream of scanned tokens
lexer :: String -> Either ScanError [ScanItem Token]
lexer input = scan [] ((1, 1), input)

-- Recursive scan until whole input has been consumed
scan :: [ScanItem Token] -> Input -> Either ScanError [ScanItem Token]
scan prevItems input = case getNextToken input of
  Left err -> Left err
  Right ((_, []), _) -> Right prevItems
  Right (remInput, newItem) -> scan (prevItems ++ [newItem]) remInput

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
matchNumberLiteral Start c 0
  | isNumber c = Just (End IntegerLiteral)
matchNumberLiteral (End IntegerLiteral) c _
  | isNumber c || c == '.' = Just (End IntegerLiteral)
matchNumberLiteral _ _ _ = Nothing

matchTypeName :: State -> Char -> Int -> Maybe State
matchTypeName Start c 0
  | isAlpha c && isUpper c = Just (End TypeName)
matchTypeName (End TypeName) c _
  | isAlphaNum c || isUnderscore c || isApostrophe c = Just (End TypeName)
matchTypeName _ _ _ = Nothing

matchValName :: State -> Char -> Int -> Maybe State
matchValName Start c 0
  | (isAlpha c && isLower c) || isUnderscore c = Just (End ValueName)
matchValName (End ValueName) c _
  | isAlphaNum c || isUnderscore c || isApostrophe c = Just (End ValueName)
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
      [ -- reservedid:
        ("case", Tokens.Case),
        ("class", Tokens.Class),
        ("data", Tokens.Data),
        ("default", Tokens.Default),
        ("deriving", Tokens.Deriving),
        ("do", Tokens.Do),
        ("else", Tokens.Else),
        ("foreign", Tokens.Foreign),
        ("if", Tokens.If),
        ("in", Tokens.In),
        ("infix", Tokens.Infix),
        ("infixl", Tokens.Infixl),
        ("infixr", Tokens.Infixr),
        ("instance", Tokens.Instance),
        ("let", Tokens.Let),
        ("module", Tokens.Module),
        ("newtype", Tokens.NewType),
        ("import", Tokens.Import),
        ("of", Tokens.Of),
        ("then", Tokens.Then),
        ("type", Tokens.Type),
        ("where", Tokens.Where),
        -- Not in Lexical Syntax
        ("as", Tokens.As),
        ("hiding", Tokens.Hiding),
        ("qualified", Tokens.Qualified)
      ]

matchers :: [Matcher]
matchers =
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
    ++ [ generateMatcher "->" SingleArrow,
         generateMatcher "=>" DoubleRightArrow,
         generateMatcher "<-" LeftArrow,
         generateMatcher "::" DoubleColon,
         generateMatcher ".." DoubleDot,
         generateMatcher " " Space,
         generateMatcher "\n" NewLine,
         --         generateMatcher "!" Exclamation,
         --         generateMatcher "#" Hash,
         --         generateMatcher "$" Dollar,
         --         generateMatcher "%" Percent,
         --         generateMatcher "&" Ampersand,
         --         generateMatcher "*" Asterisk,
         --         generateMatcher "+" Plus,
         generateMatcher "." Dot,
         --         generateMatcher "/" Divide,
         --         generateMatcher "<" LeftAngle,
         generateMatcher "=" Equals,
         --         generateMatcher ">" RightAngle,
         --         generateMatcher "?" Question,
         generateMatcher "@" At,
         generateMatcher "\\" Backslash,
         --         generateMatcher "^" Caret,
         generateMatcher "|" Pipe,
         --         generateMatcher "-" Dash,
         generateMatcher "~" Tilde,
         generateMatcher ":" Colon,
         generateMatcher ";" SemiColon,
         generateMatcher "," Comma,
         generateMatcher "`" BackTick,
         generateMatcher "(" LeftParan,
         generateMatcher ")" RightParan,
         generateMatcher "[" LeftBracket,
         generateMatcher "]" RightBracket,
         generateMatcher "{" LeftBrace,
         generateMatcher "}" RightBrace,
         generateMatcher "_" Underscore
       ]

simpleScanners :: [Scanner]
simpleScanners = map simpleMatchScanner matchers

applyMatcher :: Matcher -> State -> String -> Int -> Maybe (Token, Int)
applyMatcher matcher prevState (c : cs) i
  | isJust newState = applyMatcher matcher (fromJust newState) cs (i + 1)
  | otherwise = fmap (,i) (getToken prevState)
  where
    newState = matcher prevState c i
applyMatcher matcher prevState [] i = Just (EOF, i)

simpleMatchScanner :: Matcher -> Scanner
simpleMatchScanner m = Scanner fn
  where
    fn ((r, c), inputStr) = case applyMatcher m Start inputStr 0 of
      Just (tok, n) ->
        Right
          ( (if tok == NewLine then (r + 1, 1) else (r, c + n), drop n inputStr),
            ScanItem (r, c) (take n inputStr) tok
          )
      Nothing -> Left ScanError

withCondition :: (String -> Bool) -> Scanner -> Scanner
withCondition pred scanner = Scanner fn
  where
    fn input = case runScanner scanner input of
      Left err -> Left err
      Right (remInput, si) ->
        if pred (scanStr si)
          then Right (remInput, si)
          else Left ScanError

varsymScanner :: Scanner
varsymScanner = withCondition (fnot (isReservedOp `fand` isDashes)) scanner
  where
    matcher :: Matcher
    matcher Start c 0
      | c /= ':' && isSymbol c = Just (End Varsym)
    matcher (End Varsym) c _
      | isSymbol c = Just (End Varsym)
    matcher _ _ _ = Nothing
    scanner = simpleMatchScanner matcher

consymScanner :: Scanner
consymScanner = withCondition isReservedOp scanner
  where
    matcher :: Matcher
    matcher Start ':' 0 = Just (End Consym)
    matcher (End Consym) c _
      | isSymbol c = Just (End Consym)
    matcher _ _ _ = Nothing
    scanner = simpleMatchScanner matcher

otherScanner :: Scanner
otherScanner = simpleMatchScanner matchOther

complexScanners :: [Scanner]
complexScanners =
  [ varsymScanner,
    consymScanner
  ]

allScanners :: [Scanner]
allScanners =
  simpleScanners
    ++ complexScanners
    ++ [otherScanner] -- has to be last scanner, because it matches on everything

-- Get next token from the input
getNextToken :: Input -> Either ScanError (Input, ScanItem Token)
getNextToken input = findLongestScan input allScanners

-- Apply a list of scanners to the input and return the longest scan result
findLongestScan :: Input -> [Scanner] -> Either ScanError (Input, ScanItem Token)
findLongestScan input scanners = maybe (Left ScanError) Right longestScan
  where
    longestScan = maxBy (\(_, si) -> length (scanStr si)) (rights appliedScanners)
    appliedScanners = map (`runScanner` input) scanners

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
