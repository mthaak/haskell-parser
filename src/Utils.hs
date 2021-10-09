module Utils
  ( headMaybe,
    firstJust,
    prettyprint,
    sublist,
    maxBy,
    for,
    fand,
    fnot,
  )
where

import Data.Maybe
  ( isJust,
  )
import Data.Strings
  ( strNull,
    strTrim,
  )

-- Returns the head of a list of maybes, or Nothing if empty
headMaybe :: [Maybe a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : xs) = x

-- Returns the first Maybe which is Just, else Nothing
firstJust :: [Maybe a] -> Maybe a
firstJust maybes = headMaybe (filter isJust maybes)

-- Returns a nicer looking representation of a nested Haskell "show" result
prettyprint :: String -> String
prettyprint str = remEmptyLines $ fn str 0
  where
    fn (x : xs) n = case x of
      ',' -> x : '\n' : indent n ++ fn xs n
      '(' -> '\n' : indent (n + 2) ++ x : fn xs (n + 2)
      ')' -> x : fn xs (n - 2)
      '[' -> '\n' : indent (n + 2) ++ x : '\n' : indent (n + 4) ++ fn xs (n + 4)
      ']' -> '\n' : indent (n - 2) ++ x : '\n' : indent (n - 4) ++ fn xs (n - 4)
      _ -> x : fn xs n
    fn [] _ = []
    indent n = replicate n ' '
    remEmptyLines = unlines . filter (not . isEmpty) . lines
    isEmpty line = strNull (strTrim line)

sublist :: Int -> Int -> [a] -> [a]
sublist a b xs = take n (drop a xs)
  where
    n = if b >= 0 then b - a else length xs + b - a

maxBy :: (Ord b) => (a -> b) -> [a] -> Maybe a
maxBy fab xs = fst <$> foldl g Nothing xs
  where
    g Nothing x = Just (x, fab x)
    g (Just (xMax, max)) x =
      let xVal = fab x
       in if xVal > max
            then Just (x, xVal)
            else Just (xMax, max)

for :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
for f1 f2 = \x -> f1 x || f2 x

fand :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
fand f1 f2 = \x -> f1 x && f2 x

fnot :: (a -> Bool) -> (a -> Bool)
fnot f = \x -> not (f x)
