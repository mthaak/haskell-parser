module Main where

import Layout (convertLayout)
import Lexer (ScanItem (..), lexer)
import Parser (parse)
import Prescan (prescan)
import System.Environment
import System.Exit
import System.TimeIt (timeIt)
import Text.Printf
import Tokens (Token (Other))
import Utils (prettyprint)

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

  putStrLn $ either
    (const "")
    (\si -> printf "Number of non identified characters: %d" (length (filter (\st -> scanTok st == Other) si))) 
    scanItems

  putStrLn ""

  -- Make layout-insensitive
  let scanItems' = fmap convertLayout scanItems
  putStrLn "Layout-insensitive lexical analysis:"
  print $ either (const "error") show scanItems'

  putStrLn ""

  -- Run parser
  let parsed = fmap parse scanItems'
  putStrLn "Parse result:"
  putStrLn . either show (prettyprint . show) $ parsed
