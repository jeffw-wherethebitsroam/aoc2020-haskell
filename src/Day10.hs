module Day10
  ( run,
  )
where

import Data.List

-- last diff is always 3
diff :: [Int] -> [Int]
diff xs = zipWith (-) xs (0 : xs) ++ [3]

paths :: [Int] -> [(Int, Int)]
paths = foldl' blah [(0, 1)]

blah :: [(Int, Int)] -> Int -> [(Int, Int)]
blah acc a = do
  let p = (sum . map snd . filter reachable) acc
  -- optimisation: we need a max of the last 3 values in the acc list
  (a, p) : take 2 acc
  where
    -- all reachable previous states
    reachable x = a <= fst x + 3

run :: IO ()
run = do
  content <- readFile "../day10.txt"
  let x = map (\b -> read b :: Int) (lines content)
  let y = sort x

  let d = diff y
  let ones = (length . filter (== 1)) d
  let threes = (length . filter (== 3)) d
  print (ones * threes)

  print (head (paths y))