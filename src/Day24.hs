module Day24
  ( run,
  )
where

import Data.List

parseLine [] = []
parseLine (c : cs) = case c of
  'e' -> (2, 0) : parseLine cs
  'w' -> (-2, 0) : parseLine cs
  ns -> parseNS ns cs

parseNS ns (c : cs) = case (ns, c) of
  ('n', 'e') -> (1, 1) : parseLine cs
  ('n', 'w') -> (-1, 1) : parseLine cs
  ('s', 'e') -> (1, -1) : parseLine cs
  ('s', 'w') -> (-1, -1) : parseLine cs

groupCount = map (\xs@(x : _) -> (x, length xs)) . group . sort

add (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

neighbours x = map (add x) [(2, 0), (-2, 0), (1, 1), (-1, 1), (1, -1), (-1, -1)]

days 0 blk = blk
days n blk = do
  let nbrs = concatMap neighbours blk
  let grp = groupCount nbrs
  let newBlack = map fst (filter (isBlack blk) grp)
  days (n -1) newBlack

isBlack blk (x, cnt) = if x `elem` blk then cnt > 0 && cnt <= 2 else cnt == 2

run :: IO ()
run = do
  content <- readFile "../day24.txt"
  let dirs = map parseLine (lines content)
  let ids = map (foldr1 add) dirs
  let groups = groupCount ids

  -- we only want odd flips
  let oddFlips = filter (\x -> (snd x `mod` 2) /= 0) groups
  print (length oddFlips)

  -- part 2
  let black = map fst oddFlips
  let next = days 100 black
  print (length next)
