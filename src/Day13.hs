module Day13
  ( run,
  )
where

import Data.Bifunctor
import Data.List.Split

load1 :: [String] -> (Int, [Int])
load1 (s : ss) = do
  let ys = map read (filter (/= "x") (splitOn "," (head ss)))
  (read s, ys)

load2 :: [String] -> [(Int, Int)]
load2 (_ : ss) = do
  let ys = zip [0 ..] (splitOn "," (head ss))
  let ys' = filter (\x -> snd x /= "x") ys
  let ys'' = map (second read) ys'
  let ys''' = map (\(a, b) -> ((b - a) `mod` b, b)) ys''
  ys'''

findSoln (a1, b1) (a2, b2) = if a1 `mod` b2 == a2 then (a1, b1 * b2) else findSoln (a1 + b1, b1) (a2, b2)

solve (i : is) = foldl findSoln i is

run :: IO ()
run = do
  content <- readFile "../day13.txt"
  let i1 = load1 (lines content)

  let x = map (\i -> (i, i - fst i1 `mod` i)) (snd i1)
  let mn = foldl (\m i -> if snd i < snd m then i else m) (100, 100) x
  print (uncurry (*) mn)

  let i2 = load2 (lines content)
  print i2

  let soln = solve i2
  print soln
