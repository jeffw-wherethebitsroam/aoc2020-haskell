module Day9
  ( run,
  )
where

import Data.List

sums :: [Int] -> [Int]
sums (p : ps) = map (+ p) ps ++ sums ps
sums [] = []

exec :: [Int] -> [Int] -> Int
exec (p : ps) (d : ds) = do
  case find (== d) (sums (p : ps)) of
    Just _ -> exec (ps ++ [d]) ds
    Nothing -> d

findSum :: Int -> [Int] -> [Int]
findSum s (d : ds) = do
  case test s (d : ds) 1 of
    Just x -> x
    Nothing -> findSum s ds

test :: Int -> [Int] -> Int -> Maybe [Int]
test s ds spl = do
  let xs = take spl ds
  let sm = sum xs
  if sm == s
    then Just xs
    else
      if sm < s
        then test s ds (spl + 1)
        else Nothing

run :: IO ()
run = do
  content <- readFile "../day9.txt"
  let x = map (\b -> read b :: Int) (lines content)

  let (p, d) = splitAt 25 x
  let p1 = exec p d
  print p1

  let p2 = findSum p1 x
  print (minimum p2 + maximum p2)