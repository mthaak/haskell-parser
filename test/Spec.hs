import ParserTest
import RunTest
import Test.HUnit

tests :: Test
tests =
  TestList
    [ parserTests,
      runTests
    ]

main :: IO Counts
main = runTestTT tests
