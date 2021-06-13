import ParserTest
import Test.HUnit

tests :: Test
tests = TestList [parserTests]

main :: IO Counts
main = runTestTT tests
