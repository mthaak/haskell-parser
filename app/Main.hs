module Main where

import Prescan (prescan)
import Layout (convertLayout)
import Lexer (ScanItem (..), lexer)
import Parser (parse)
import System.Environment
import System.Exit
import Text.Printf
import Tokens (Token (Other))
import Utils (prettyprint)
import System.TimeIt (timeIt)

main :: IO ()
main = timeIt $ getArgs >>= parseArgs >>= run

parseArgs :: [FilePath] -> IO [Char]
parseArgs [] = usage >> exitSuccess
parseArgs fs = concat `fmap` mapM readFile fs

usage :: IO ()
usage = putStrLn "Usage: haskell-parser [FILEPATH]"

run :: String -> IO ()
run input = do
  putStrLn "Running..."

  putStrLn ""

  -- Prescan
  let (prescanned, pragmas) = prescan input
  putStrLn . printf "Pragmas: %s" $ show pragmas

  putStrLn ""

  -- Run lexical analysis
  let scanItems = lexer prescanned
  putStrLn . printf "Number of non identified characters: %d" $
    length (filter (\st -> scanTok st == Other) scanItems)

  putStrLn ""

  -- Make layout-insensitive
  let scanItems' = convertLayout scanItems
  putStrLn "Layout-insensitive lexical analysis:"
  print scanItems'

  putStrLn ""

  -- Run parser
  let parsed = parse scanItems'
  putStrLn "Parse result:"
  putStrLn . either show (prettyprint . show) $ parsed
