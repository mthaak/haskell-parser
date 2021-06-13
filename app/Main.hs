module Main where

import Layout (convertLayout)
import Lexer (lexer)
import Parser (parseTokens)
import System.Environment
import System.Exit
import Text.Printf
import Tokens (Token (Other))
import Utils (prettyprint)

main :: IO ()
main = getArgs >>= parseArgs >>= run

parseArgs :: [FilePath] -> IO [Char]
parseArgs [] = usage >> exitSuccess
parseArgs fs = concat `fmap` mapM readFile fs

usage :: IO ()
usage = putStrLn "Usage: haskell-parser [FILEPATH]"

run :: String -> IO ()
run input = do
  putStrLn "Running..."

  putStrLn ""

  -- Scan tokens
  let tokens = lexer input
  putStrLn . printf "Number of non identified characters: %d" $
    length (filter (\(tok, _, _) -> tok == Other) tokens)

  putStrLn ""

  -- Make layout-insensitive
  let tokens' = convertLayout tokens
  putStrLn "Layout-insensitive lexical analysis:"
  print tokens'

  putStrLn ""

  -- Parse
  let parsed = parseTokens tokens'
  putStrLn "Parse result:"
  putStrLn . prettyprint . either (const "Error") show $ parsed
