module ParserHelpers where

import Common (Coordinates)
import Control.Applicative
import Control.Monad
import Elements
import Lexer
import Text.Printf (printf)
import Tokens (KeywordToken (..), Token (..))

type Input = [ScanItem Token]

newtype Parser a = Parser {runParser :: Input -> Either ParseError (a, Input)}

instance Functor Parser where
  fmap fab pa = Parser $ fmap fn . runParser pa
    where
      fn (a, input) = (fab a, input)

instance Applicative Parser where
  pure match = Parser $ \input -> Right (match, input)
  p1 <*> p2 =
    Parser $ \input -> do
      (fab, notParsed) <- runParser p1 input
      (a, notParsed') <- runParser p2 notParsed
      return (fab a, notParsed')

unit :: a -> Parser a
unit match = Parser $ \input -> Right (match, input)

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ runParser p >=> fn
  where
    fn (match, remaining) = runParser (f match) remaining

instance Monad Parser where
  return = unit
  (>>=) = bind

failure :: Parser a
failure = Parser $ \s -> Left $ ParseError (71, 0) "Failure"

-- Run first parser. If fail, run second parser
option :: Parser a -> Parser a -> Parser a
option pa1 pa2 = Parser $ \s ->
  case runParser pa1 s of
    res@(Right _) -> res
    Left err1 ->
      case runParser pa2 s of
        res@(Right _) -> res
        Left err2 ->
          if errorLoc err1 >= errorLoc err2
            then Left err1
            else Left err2

instance Alternative Parser where
  empty = failure
  (<|>) = option

data ParseError = ParseError Coordinates String deriving (Eq, Show)

errorLoc :: ParseError -> Coordinates
errorLoc (ParseError c _) = c

errorMsg :: ParseError -> String
errorMsg (ParseError _ s) = s

-- PARSERS HELPERS

fail :: Either ParseError a
fail = Left $ ParseError (0, 0) "Fail"

parseFail :: Parser a
parseFail = Parser $ const ParserHelpers.fail

parseNothing :: Parser (Maybe a)
parseNothing = Parser $ \s -> Right (Nothing, s)

parseNop :: Parser Nop
parseNop = Parser $ \s -> Right (Nop, s)

parseItem :: Token -> Parser (ScanItem Token)
parseItem expTok = Parser fn
  where
    fn (si : xs)
      | scanTok si == expTok = Right (si, xs)
      | otherwise = Left $ ParseError (0, 0) (printf "Could not parse item %s as expected token %s" (show si) (show expTok))
    fn [] = Left $ ParseError (0, 0) "Could not find next scan item"

parseToken :: Token -> Parser Token
parseToken tok = scanTok <$> parseItem tok

parseTokenAsString :: Token -> Parser String
parseTokenAsString tok = scanStr <$> parseItem tok

parseTokens :: [Token] -> Parser [Token]
parseTokens = mapM parseToken

parseKeyword :: KeywordToken -> Parser KeywordToken
parseKeyword keyword = parser >>= toKWParser
  where
    parser = parseToken (Keyword keyword)
    toKWParser tok =
      case tok of
        Keyword kw -> Parser (\input -> Right (kw, input))
        _ -> parseFail

either :: Parser a -> Parser b -> Parser (Either a b)
either pa pb = Left <$> pa <|> Right <$> pb

oneOfTokens :: [Token] -> Parser Token
oneOfTokens tokens = foldl1 (<|>) (map parseToken tokens)

oneOfTokensAsString :: [Token] -> Parser String
oneOfTokensAsString tokens = foldl1 (<|>) (map parseTokenAsString tokens)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore pa = Parser $ \s -> fn s []
  where
    fn (x : xs) matches =
      case nextResult of
        Left (ParseError _ _) -> Right (matches, x : xs)
        Right (match, remaining) -> fn remaining (matches ++ [match])
      where
        nextResult = runParser pa (x : xs)
    fn [] matches = Right (matches, [])

oneOrMore :: Parser a -> Parser [a]
oneOrMore pa = do
  x <- pa
  xs <- zeroOrMore pa
  pure (x : xs)

zeroOrMoreSep :: Token -> Parser a -> Parser [a]
zeroOrMoreSep t pa = oneOrMoreSep t pa <|> pure []

oneOrMoreSep :: Token -> Parser a -> Parser [a]
oneOrMoreSep t pa = do
  x <- pa
  xs <- zeroOrMore (pa `precededBy` t)
  pure (x : xs)

-- zeroOrMoreSep with mandatory first separator
zeroOrMoreSep2 :: Token -> Parser a -> Parser [a]
zeroOrMoreSep2 t pa = zeroOrMore (pa `precededBy` t)

-- oneOrMoreSep with mandatory first separator
oneOrMoreSep2 :: Token -> Parser a -> Parser [a]
oneOrMoreSep2 t pa = oneOrMore (pa `precededBy` t)

tupled :: Parser a -> Parser [a]
tupled pa = do
  parseToken Tokens.LeftParan
  elems <- oneOrMoreSep Tokens.Comma pa
  parseToken Tokens.RightParan
  pure elems

-- Allows extra comma at the end
tupled' :: Parser a -> Parser [a]
tupled' pa = do
  parseToken Tokens.LeftParan
  elems <- oneOrMoreSep Tokens.Comma pa
  optional $ parseToken Tokens.Comma
  parseToken Tokens.RightParan
  pure elems

listed :: Parser a -> Parser [a]
listed pa = do
  parseToken Tokens.LeftBracket
  elems <- oneOrMoreSep Tokens.Comma pa
  parseToken Tokens.RightBracket
  pure elems

mkList :: Parser a -> Parser [a]
mkList = fmap (\x -> [x])

betweenParans :: Parser a -> Parser a
betweenParans pa = between pa Tokens.LeftParan Tokens.RightParan

betweenBraces :: Parser a -> Parser a
betweenBraces pa = between pa Tokens.LeftBrace Tokens.RightBrace

betweenBrackets :: Parser a -> Parser a
betweenBrackets pa = between pa Tokens.LeftBracket Tokens.RightBracket

betweenTicks :: Parser a -> Parser a
betweenTicks pa = between pa Tokens.BackTick Tokens.BackTick

parseEmpty :: a -> Parser a
parseEmpty a = Parser $ \s -> Right (a, s)

precededBy :: Parser a -> Token -> Parser a
precededBy pa t = do
  parseToken t
  pa

precededByOpt :: Parser a -> Token -> Parser a
precededByOpt pa t = do
  optional $ parseToken t
  pa

followedBy :: Parser a -> Token -> Parser a
followedBy pa t = do
  result <- pa
  parseToken t
  return result

followedByOpt :: Parser a -> Token -> Parser a
followedByOpt pa t = do
  result <- pa
  optional $ parseToken t
  return result

precedes :: Token -> Parser a -> Parser a
precedes t pa = do
  parseToken t
  pa

between :: Parser a -> Token -> Token -> Parser a
between pa l r = do
  parseToken l
  a <- pa
  parseToken r
  return a

trailingSemicolon :: Parser a -> Parser a
trailingSemicolon pa = do
  a <- pa
  optional (parseToken SemiColon)
  return a

parseAll :: Parser All
parseAll = do
  betweenParans (parseToken DoubleDot)
  return All

failIf :: (a -> Bool) -> Parser a -> Parser a
failIf cond pa = do
  x <- pa
  if cond x then pure x else failure
