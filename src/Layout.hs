module Layout
  ( convertLayout,
  )
where

import Common (Coordinates)
import Data.Either (fromRight, isLeft, isRight, rights)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (traceShow, traceShowId)
import Tokens

adjustLayout :: [(Token, String)] -> [(Token, String)]
adjustLayout = map m . filter f
  where
    f = isNotSpace
    isNotSpace (t, s) = t /= Space
    m = replaceNewLine
    replaceNewLine (t, s) = if t == NewLine then (SemiColon, ";") else (t, s)

convertLayout :: [(Token, String, Coordinates)] -> [(Token, String)]
convertLayout input = lexemes
  where
    tokensWithIndent = traceShowId $ addIndentIndicators input False
    tokensMappedWithL = traceShowId $ funL tokensWithIndent []
    tokensCleaned = traceShowId $ cleanLayout (fromRight [] tokensMappedWithL)
    lexemes = traceShowId $ filterLexemes tokensCleaned

cleanLayout :: [Either IndentIndicator (Token, String)] -> [(Token, String)]
cleanLayout = rights

filterLexemes :: [(Token, String)] -> [(Token, String)]
filterLexemes = filter isLexeme
  where
    isLexeme (t, _) = tokenIsLexeme t

-- TODO merge into scanner, use ScanItem { scanLoc :: Location , scanStr :: String , scanItem :: a }

data IndentIndicator
  = Start Int -- start context {n}
  | Indent Int -- line indent < n >
  deriving (Eq, Show)

tokenIsLexeme :: Token -> Bool
tokenIsLexeme tok = tok `notElem` [Space, NewLine, LineComment, EOF]

addIndentIndicators :: [(Token, String, Coordinates)] -> Bool -> [Either IndentIndicator (Token, String)]
-- If a let, where, do, or of keyword is not followed by the lexeme {,
-- the token {n} is inserted after the keyword,
-- where n is the indentation of the next lexeme if there is one,
-- or 0 if the end of file has been reached.
addIndentIndicators ((tok, str, _) : tokens) hasStart
  | tok `elem` [Keyword Let, Keyword Where, Keyword Do, Keyword Of]
      && nextLexemeExists
      && isNotLeftBrace nextLexeme =
    if isEOF nextLexeme
      then Right (tok, str) : [Left (Start 1)]
      else Right (tok, str) : Left (Start (getCol nextLexeme)) : addIndentIndicators tokens True
  where
    maybeNextLexeme = find (\(t, _, _) -> tokenIsLexeme t) tokens
    nextLexemeExists = isJust maybeNextLexeme
    nextLexeme = fromJust maybeNextLexeme
    isNotLeftBrace (t, _, _) = t /= LeftBrace
    isEOF (t, _, _) = t == EOF
    getCol (_, _, (_, col)) = col
-- Where the start of a lexeme is preceded only by white space on the same line,
-- this lexeme is preceded by < n > where n is the indentation of the lexeme,
-- provided that it is not, as a consequence of the first two rules, preceded by {n}
addIndentIndicators ((tok, str, (r, c)) : tokens) False
  | startsNewLine && nextLexemeExists && nextLexemeRow == r + 1 =
    Right (tok, str) : Left (Indent nextLexemeColumn) : addIndentIndicators tokens False
  where
    startsNewLine = tok == NewLine || tok == LineComment
    maybeNextLexeme = find (\(t, _, _) -> tokenIsLexeme t) tokens
    nextLexemeExists = isJust maybeNextLexeme
    (nextLexemeRow, nextLexemeColumn) = getCoord (fromJust maybeNextLexeme)
    getCoord (_, _, coord) = coord
addIndentIndicators ((t, s, _) : tokens) hasStart = Right (t, s) : addIndentIndicators tokens keepStart
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

funL :: [Either IndentIndicator (Token, String)] -> [Int] -> Either LayoutError [Either IndentIndicator (Token, String)]
-- L (< n >: ts) (m : ms) if m = n = ; : (L ts (m : ms))
funL (Left (Indent n) : ts) (m : ms) | m == n = Right (SemiColon, ";") >: funL ts (m : ms)
-- L (< n >: ts) (m : ms) if n < m = } : (L (< n >: ts) ms)
funL (Left (Indent n) : ts) (m : ms) | n < m = Right (RightBrace, "}") >: funL (Left (Indent n) : ts) ms
-- L (< n >: ts) ms = L ts ms
funL (Left (Indent n) : ts) ms = funL ts ms
-- L ({n} : ts) (m : ms) if n > m = { : (L ts (n : m : ms))
funL (Left (Start n) : ts) (m : ms) | n > m = Right (LeftBrace, "{") >: funL ts (n : m : ms)
-- L ({n} : ts) [] if n > 0 = { : (L ts [n])
funL (Left (Start n) : ts) [] | n > 0 = Right (LeftBrace, "{") >: funL ts [n]
-- L ({n} : ts) ms = { : } : (L (< n >: ts) ms)
funL (Left (Start n) : ts) ms = Right (LeftBrace, "{") >: (Right (RightBrace, "}") >: funL (Left (Indent n) : ts) ms)
-- L (} : ts) (0 : ms) = } : (L ts ms)
funL (Right (RightBrace, "}") : ts) (0 : ms) = Right (RightBrace, "}") >: funL ts ms
-- L (} : ts) ms = parse-error
funL (Right (RightBrace, "}") : ts) ms = Left LayoutError
-- L ({ : ts) ms = { : (L ts (0 : ms))
funL (Right (LeftBrace, "{") : ts) ms = Right (LeftBrace, "{") >: funL ts (0 : ms)
-- L (t : ts) (m : ms) if m ∕= 0 and parse-error(t) = } : (L (t : ts) ms)
funL (t : ts) (m : ms) | m /= 0 && parseError = Right (RightBrace, "}") >: funL (t : ts) ms
  where
    parseError = isLeft (funL (t : ts) ms) && isRight (funL (Right (RightBrace, "}") : t : ts) ms)
-- L (t : ts) ms = t : (L ts ms)
funL (Right t : ts) ms = Right t >: funL ts ms
-- L [] [] = []
funL [] [] = Right []
-- L [] (m : ms) = } : L [] ms
funL [] (m : ms) = Right (RightBrace, "}") >: funL [] ms
