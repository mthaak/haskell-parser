module Layout
  ( convertLayout,
  )
where

import Common (Coordinates, Error (..))
import Data.Either (fromRight, rights)
import Data.List (find)
import Data.Maybe (fromJust, isJust, isNothing)
import Lexer (ScanItem (..))
import Tokens

convertLayout :: [ScanItem Token] -> [ScanItem Token]
convertLayout input = tokensCleaned
  where
    tokensWithIndent = addIndentIndicators input False
    lexemes = filterLexemes tokensWithIndent
    tokensMappedWithL = funL lexemes []
    tokensCleaned = cleanLayout (fromRight [] tokensMappedWithL)

-- Remove indent indicators
cleanLayout :: [Either IndentIndicator (ScanItem Token)] -> [ScanItem Token]
cleanLayout = rights

filterLexemes :: [Either IndentIndicator (ScanItem Token)] -> [Either IndentIndicator (ScanItem Token)]
filterLexemes = filter isLexeme
  where
    isLexeme si = either (const True) (tokenIsLexeme . scanTok) si

-- TODO merge into scanner, use ScanItem { scanLoc :: Coordinates , scanStr :: String , scanItem :: a }

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
      && (isEOF || isNotLeftBrace nextLexeme) =
    if isEOF
      then Right item : [Left (Start 0)]
      else Right item : Left (Start (getCol nextLexeme)) : addIndentIndicators items True
  where
    maybeNextLexeme = find (tokenIsLexeme . scanTok) items
    isEOF = isNothing maybeNextLexeme
    nextLexeme = fromJust maybeNextLexeme
    isNotLeftBrace si = scanTok si /= LeftBrace
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

-- Appends elem to list if is right, otherwise return left
-- TODO use monad for this?
(>:) :: b -> Either a [b] -> Either a [b]
(>:) b (Right bs) = Right (b : bs)
(>:) _ (Left bs) = Left bs

-- Special coordinates for inserted tokens
x :: Coordinates
x = (0, 0)

funL :: [Either IndentIndicator (ScanItem Token)] -> [Int] -> Either Error [Either IndentIndicator (ScanItem Token)]
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
funL (Right (ScanItem coo _ RightBrace) : ts) ms = Left (LayoutError coo "Layout error")
-- L ({ : ts) ms = { : (L ts (0 : ms))
funL (Right (ScanItem _ _ LeftBrace) : ts) ms = Right (ScanItem x "{" LeftBrace) >: funL ts (0 : ms)
-- L (t : ts) (m : ms) if m ∕= 0 and parse-error(t) = } : (L (t : ts) ms)
-- TODO fix this condition
-- Luckily the program still works when disabling it, but it's not clear in which cases it won't work
--funL (t : ts) (m : ms) | m /= 0 && parseError = Right (ScanItem x "}" RightBrace) >: funL (t : ts) ms
--  where
--    numCurrentOpenContexts = length (m : ms)
--    futureTokens = map scanTok (rights ts)
--    numFutureContextsWillOpen = length (filter (== LeftBrace) futureTokens)
--    numFutureContextsWillClose = length (filter (== RightBrace) futureTokens)
--    parseError = traceShow (numCurrentOpenContexts, numFutureContextsWillOpen, numFutureContextsWillClose) $ numCurrentOpenContexts > (numFutureContextsWillClose - numFutureContextsWillOpen)
-- L (t : ts) ms = t : (L ts ms)
funL (Right t : ts) ms = Right t >: funL ts ms
-- L [] [] = []
funL [] [] = Right []
-- L [] (m : ms) = } : L [] ms
funL [] (m : ms) = Right (ScanItem x "}" RightBrace) >: funL [] ms
