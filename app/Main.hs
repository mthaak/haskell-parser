module Main where

import Run (RunArgs (..), run)
import System.Environment
import System.Exit
import System.TimeIt (timeIt)

usage :: IO ()
usage = putStrLn "Usage: haskell-parser [FILEPATH]"

parseArgs :: [FilePath] -> IO [Char]
parseArgs [] = usage >> exitSuccess
parseArgs fs = concat `fmap` mapM readFile fs

defaultRunArgs :: RunArgs
defaultRunArgs = RunArgs True

omitResult :: a -> IO ()
omitResult _ = sequence_ []

main :: IO ()
main = timeIt $ getArgs >>= parseArgs >>= run defaultRunArgs >>= omitResult
