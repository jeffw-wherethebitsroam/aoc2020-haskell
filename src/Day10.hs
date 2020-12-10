module Day10
  ( run,
  )
where

import Data.List

diffs :: [Int] -> [Int]
diffs (a1 : a2 : as) = a2 - a1 : diffs (a2 : as)
-- last diff is always 3
diffs [_] = [3]

paths :: [Int] -> [(Int, Int)]
paths = paths' [(0, 1)]

paths' :: [(Int, Int)] -> [Int] -> [(Int, Int)]
paths' acc [] = acc
paths' acc (a : as) = do
  let p = (sum . map snd . reachable) acc
  paths' ((a, p) : acc) as
  where
    -- all reachable previous states
    reachable = filter (\(x, _) -> a <= x + 3)

run :: IO ()
run = do
  content <- readFile "../day10.txt"
  let x = map (\b -> read b :: Int) (lines content)
  let y = sort x

  let d = diffs (0 : y)
  let ones = (length . filter (== 1)) d
  let threes = (length . filter (== 3)) d
  print (ones * threes)

  print (paths y)