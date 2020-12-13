module Day6
  ( run,
  )
where

import Data.List
import Data.List.Split

rowCount = length . nub . filter (/= '\n')

rowCount2 :: [Char] -> Int
rowCount2 s = do
  let x = split (dropFinalBlank $ dropDelims $ oneOf "\n") s
  let counts = countDistinct (filter (/= '\n') s)
  let y = filter (\(_, c) -> c == length x) counts
  length y

countDistinct :: [Char] -> [(Char, Int)]
countDistinct = map (\xs@(x : _) -> (x, length xs)) . group . sort

rowCount2_v2 :: [Char] -> Int
rowCount2_v2 = length . nub . foldl1 intersect . split (dropFinalBlank $ dropDelims $ oneOf "\n")

run :: IO ()
run = do
  content <- readFile "../day6.txt"
  let blah = splitOn "\n\n" content
  print (sum (map rowCount blah))

  print (sum (map rowCount2 blah))
  print (sum (map rowCount2_v2 blah))