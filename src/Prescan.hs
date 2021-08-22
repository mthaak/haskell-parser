module Prescan where

import Data.List (isPrefixOf, isSuffixOf)
import Utils (sublist)

data Pragma = Language String
  deriving (Eq, Show)

data PrescanError = PrescanError

prescan :: String -> (String, [Pragma])
prescan input = (unlines lines_, pragmas)
  where
    (lines_, pragmas) = prescanLines (lines input)

prescanLines :: [String] -> ([String], [Pragma])
prescanLines = foldr fn ([], [])
  where
    fn line (prevLines, pragmas) =
      either
        (\err -> (line : prevLines, pragmas))
        (\pragma -> ("" : prevLines, pragma : pragmas)) -- empty string needed for keeping right line numbers
        (extractPragma line)

lineContainsPragma :: String -> Bool
lineContainsPragma line =
  ("{-# " `isPrefixOf` line)
    && (" #-}" `isSuffixOf` line)

extractPragma :: String -> Either PrescanError Pragma
extractPragma line
  | lineContainsPragma line =
    parsePragma (sublist 4 (-4) line)
extractPragma _ = Left PrescanError

parsePragma :: String -> Either PrescanError Pragma
parsePragma input
  | "LANGUAGE " `isPrefixOf` input =
    Language <$> parseLanguagePragma (drop 9 input)
parsePragma _ = Left PrescanError

parseLanguagePragma :: String -> Either PrescanError String
parseLanguagePragma str = Right str
