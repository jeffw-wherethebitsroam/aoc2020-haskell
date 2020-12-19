module Day17
  ( run,
  )
where

import Data.List

mapRow (y, row) = do
  let r = zip [0 ..] row
  let f = filter (\x -> snd x == '#') r
  map (\(x, c) -> (x, y, 0)) f

mapRow4d (y, row) = do
  let r = zip [0 ..] row
  let f = filter (\x -> snd x == '#') r
  map (\(x, c) -> (x, y, 0, 0)) f

explode (x, y, z) = [(i, j, k) | i <- [x -1 .. x + 1], j <- [y -1 .. y + 1], k <- [z -1 .. z + 1], (i, j, k) /= (x, y, z)]

explode4d (x, y, z, w) = [(i, j, k, l) | i <- [x -1 .. x + 1], j <- [y -1 .. y + 1], k <- [z -1 .. z + 1], l <- [w -1 .. w + 1], (i, j, k, l) /= (x, y, z, w)]

active prev (coord, count) = if coord `elem` prev then count == 2 || count == 3 else count == 3

next exp grid = do
  let m = concatMap exp grid
  let g = (map (\xs@(x : _) -> (x, length xs)) . group . sort) m
  map fst (filter (active grid) g)

exec exp 0 grid = grid
exec exp i grid = exec exp (i -1) (next exp grid)

run :: IO ()
run = do
  content <- readFile "../day17.txt"
  let rows = zip [0 ..] (lines content)
  let grid = concatMap mapRow rows
  let p1 = exec explode 6 grid
  print (length p1)

  let g4d = concatMap mapRow4d rows
  let p2 = exec explode4d 6 g4d
  print (length p2)