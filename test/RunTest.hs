module RunTest where

import Run (RunArgs (..), run)
import Test.HUnit
import Text.Printf (printf)

testRunArgs :: RunArgs
testRunArgs = RunArgs False

canRunOn :: String -> Assertion
canRunOn input = do
  output <- run testRunArgs input
  case output of
    Left err -> assertBool ("Run failed: " ++ show err) False
    Right _ -> assertBool "Run successful" True

testCanRunOnFile :: FilePath -> Test
testCanRunOnFile filepath =
  TestCase
    ( do
        input <- readFile filepath
        canRunOn input
    )

-- TODO list files from directory
sourceFiles :: [String]
sourceFiles =
  [ --  "src/Common.hs",
    --    "src/Elements.hs",
    --    "src/Layout.hs",
    --    "src/Lexer.hs",
    --    "src/Parser.hs",
    --    "src/ParserHelpers.hs",
    --    "src/Prescan.hs",
    --    "src/Run.hs",
    --    "src/Tokens.hs",
    "src/Utils.hs"
  ]

runTests :: Test
runTests =
  TestLabel
    "RunTests"
    ( TestList
        [ TestLabel
            (printf "test_canParseFile %s" sourceFile)
            (testCanRunOnFile sourceFile)
          | sourceFile <- sourceFiles
        ]
    )
