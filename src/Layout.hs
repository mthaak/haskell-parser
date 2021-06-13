module Layout
  ( convertLayout,
  )
where

import Common (Coordinates)
import Data.Either (fromRight, isLeft, isRight, rights)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (traceShowId)
import Lexer (ScanItem (..))
import Tokens

adjustLayout :: [(Token, String)] -> [(Token, String)]
adjustLayout = map m . filter f
  where
    f = isNotSpace
    isNotSpace (t, s) = t /= Space
    m = replaceNewLine
    replaceNewLine (t, s) = if t == NewLine then (SemiColon, ";") else (t, s)

convertLayout :: [ScanItem Token] -> [ScanItem Token]
convertLayout input = lexemes
  where
    tokensWithIndent = traceShowId $ addIndentIndicators input False
    tokensMappedWithL = traceShowId $ funL tokensWithIndent []
    tokensCleaned = traceShowId $ cleanLayout (fromRight [] tokensMappedWithL)
    lexemes = traceShowId $ filterLexemes tokensCleaned

-- Remove indent indicators
cleanLayout :: [Either IndentIndicator (ScanItem Token)] -> [ScanItem Token]
cleanLayout = rights

filterLexemes :: [ScanItem Token] -> [ScanItem Token]
filterLexemes = filter isLexeme
  where
    isLexeme si = tokenIsLexeme (scanTok si)

-- TODO merge into scanner, use ScanItem { scanLoc :: Location , scanStr :: String , scanItem :: a }

data IndentIndicator
  = Start Int -- start context {n}
  | Indent Int -- line indent < n >
  deriving (Eq, Show)

tokenIsLexeme :: Token -> Bool
tokenIsLexeme tok = tok `notElem` [Space, NewLine, LineComment, EOF]

addIndentIndicators :: [ScanItem Token] -> Bool -> [Either IndentIndicator (ScanItem Token)]
-- If a let, where, do, or of keyword is not followed by the lexeme {,
-- the token {n} is inserted after the keyword,
-- where n is the indentation of the next lexeme if there is one,
-- or 0 if the end of file has been reached.
addIndentIndicators (item : items) hasStart
  | scanTok item `elem` [Keyword Let, Keyword Where, Keyword Do, Keyword Of]
      && nextLexemeExists
      && isNotLeftBrace nextLexeme =
    if isEOF nextLexeme
      then Right item : [Left (Start 1)]
      else Right item : Left (Start (getCol nextLexeme)) : addIndentIndicators items True
  where
    maybeNextLexeme = find (tokenIsLexeme . scanTok) items
    nextLexemeExists = isJust maybeNextLexeme
    nextLexeme = fromJust maybeNextLexeme
    isNotLeftBrace si = scanTok si /= LeftBrace
    isEOF si = scanTok si == EOF
    getCol si = snd (scanLoc si)
-- Where the start of a lexeme is preceded only by white space on the same line,
-- this lexeme is preceded by < n > where n is the indentation of the lexeme,
-- provided that it is not, as a consequence of the first two rules, preceded by {n}
addIndentIndicators ((ScanItem (r, c) str tok) : items) False
  | startsNewLine && nextLexemeExists && nextLexemeRow == r + 1 =
    Right (ScanItem (r, c) str tok) : Left (Indent nextLexemeColumn) : addIndentIndicators items False
  where
    startsNewLine = tok == NewLine || tok == LineComment
    maybeNextLexeme = find (tokenIsLexeme . scanTok) items
    nextLexemeExists = isJust maybeNextLexeme
    (nextLexemeRow, nextLexemeColumn) = scanLoc (fromJust maybeNextLexeme)
addIndentIndicators ((ScanItem c s t) : items) hasStart = Right (ScanItem c s t) : addIndentIndicators items keepStart
  where
    keepStart = hasStart && (t == Space || t == NewLine || t == LineComment)
addIndentIndicators [] _ = []

-- TODO string literal spanning multiple lines
-- TODO If the first lexeme of a module is not { or module, then it is preceded by {n} where n is the indentation of the lexeme.

data LayoutError = LayoutError deriving (Eq, Show)

-- Appends elem to list if is right, otherwise return left
-- TODO use monad for this?
(>:) :: b -> Either a [b] -> Either a [b]
(>:) b (Right bs) = Right (b : bs)
(>:) _ (Left bs) = Left bs

-- Special coordinates for inserted tokens
x :: Coordinates
x = (0, 0)

funL :: [Either IndentIndicator (ScanItem Token)] -> [Int] -> Either LayoutError [Either IndentIndicator (ScanItem Token)]
-- L (< n >: ts) (m : ms) if m = n = ; : (L ts (m : ms))
funL (Left (Indent n) : ts) (m : ms) | m == n = Right (ScanItem x ";" SemiColon) >: funL ts (m : ms)
-- L (< n >: ts) (m : ms) if n < m = } : (L (< n >: ts) ms)
funL (Left (Indent n) : ts) (m : ms) | n < m = Right (ScanItem x "}" RightBrace) >: funL (Left (Indent n) : ts) ms
-- L (< n >: ts) ms = L ts ms
funL (Left (Indent n) : ts) ms = funL ts ms
-- L ({n} : ts) (m : ms) if n > m = { : (L ts (n : m : ms))
funL (Left (Start n) : ts) (m : ms) | n > m = Right (ScanItem x "{" LeftBrace) >: funL ts (n : m : ms)
-- L ({n} : ts) [] if n > 0 = { : (L ts [n])
funL (Left (Start n) : ts) [] | n > 0 = Right (ScanItem x "{" LeftBrace) >: funL ts [n]
-- L ({n} : ts) ms = { : } : (L (< n >: ts) ms)
funL (Left (Start n) : ts) ms = Right (ScanItem x "{" LeftBrace) >: (Right (ScanItem x "}" RightBrace) >: funL (Left (Indent n) : ts) ms)
-- L (} : ts) (0 : ms) = } : (L ts ms)
funL (Right (ScanItem _ _ RightBrace) : ts) (0 : ms) = Right (ScanItem x "}" RightBrace) >: funL ts ms
-- L (} : ts) ms = parse-error
funL (Right (ScanItem _ _ RightBrace) : ts) ms = Left LayoutError
-- L ({ : ts) ms = { : (L ts (0 : ms))
funL (Right (ScanItem _ _ LeftBrace) : ts) ms = Right (ScanItem x "{" LeftBrace) >: funL ts (0 : ms)
-- L (t : ts) (m : ms) if m ∕= 0 and parse-error(t) = } : (L (t : ts) ms)
funL (t : ts) (m : ms) | m /= 0 && parseError = Right (ScanItem x "}" RightBrace) >: funL (t : ts) ms
  where
    parseError = isLeft (funL (t : ts) ms) && isRight (funL (Right (ScanItem x "}" RightBrace) : t : ts) ms)
-- L (t : ts) ms = t : (L ts ms)
funL (Right t : ts) ms = Right t >: funL ts ms
-- L [] [] = []
funL [] [] = Right []
-- L [] (m : ms) = } : L [] ms
funL [] (m : ms) = Right (ScanItem x "}" RightBrace) >: funL [] ms
