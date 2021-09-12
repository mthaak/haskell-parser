module Run
  ( run,
    RunArgs (..),
  )
where

import Common (Error)
import Control.Monad (when)
import Layout (convertLayout)
import Lexer (ScanItem (..), lexer)
import Parser (ParseResult, parse)
import Prescan (prescan)
import Text.Printf
import Tokens (Token (Other))
import Utils (prettyprint)

newtype RunArgs = RunArgs {runVerbose :: Bool}

run :: RunArgs -> String -> IO (Either Error ParseResult)
run args input = do
  when
    (runVerbose args)
    ( do
        putStrLn "Running..."
        putStrLn ""
    )

  -- Prescan
  let (prescanned, pragmas) = prescan input
  when
    (runVerbose args)
    ( do
        putStrLn . printf "Pragmas: %s" $ show pragmas
        putStrLn ""
    )

  -- Run lexical analysis
  let scanItems = lexer prescanned
  when
    (runVerbose args)
    ( do
        putStrLn $
          either
            (const "")
            (\si -> printf "Number of non identified characters: %d" (length (filter (\st -> scanTok st == Other) si)))
            scanItems
        putStrLn ""
    )

  -- Make layout-insensitive
  let scanItems' = fmap convertLayout scanItems
  when
    (runVerbose args)
    ( do
        putStrLn "Layout-insensitive lexical analysis:"
        putStrLn $ either (const "error") show scanItems'
        putStrLn ""
    )

  -- Run parser
  let parsed = scanItems' >>= parse
  when
    (runVerbose args)
    ( do
        putStrLn "Parse result:"
        putStrLn . either show (prettyprint . show) $ parsed
    )

  pure parsed
