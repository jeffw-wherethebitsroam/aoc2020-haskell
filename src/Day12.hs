module Day12
  ( run,
  )
where

import Array
import Debug.Trace

-- ship x,y, dir
type State = (Int, Int, Int)

-- waypoint x,y, ship x,y
type State2 = (Int, Int, Int, Int)

type Op = (Char, Int)

readOp :: [Char] -> Op
readOp (c : cs) = (c, read cs)

dirs = array (0, 3) [(0, 'N'), (1, 'E'), (2, 'S'), (3, 'W')]

mdist (x, y, _) = abs x + abs y

mdist2 (_, _, x, y) = abs x + abs y

turn :: Int -> State -> State
turn n (x, y, d) = do
  let d' = (d + (n `div` 90) + 4) `mod` 4
  (x, y, d')

apply2d s o = trace ("state: " ++ show s ++ ", op: " ++ show o) apply2 s o

apply :: State -> Op -> State
apply (x, y, d) (oc, n) = case oc of
  'N' -> (x, y + n, d)
  'S' -> (x, y - n, d)
  'E' -> (x + n, y, d)
  'W' -> (x - n, y, d)
  'F' -> apply (x, y, d) (dirs ! d, n)
  'L' -> turn (- n) (x, y, d)
  'R' -> turn n (x, y, d)

turn2 n (wx, wy, sx, sy) = case n of
  90 -> (wy, - wx, sx, sy)
  180 -> (- wx, - wy, sx, sy)
  270 -> (- wy, wx, sx, sy)
  360 -> (wx, wy, sx, sy)

apply2 :: State2 -> Op -> State2
apply2 (wx, wy, sx, sy) (oc, n) = case oc of
  'N' -> (wx, wy + n, sx, sy)
  'S' -> (wx, wy - n, sx, sy)
  'E' -> (wx + n, wy, sx, sy)
  'W' -> (wx - n, wy, sx, sy)
  'F' -> (wx, wy, sx + n * wx, sy + n * wy)
  'L' -> turn2 (360 - n) (wx, wy, sx, sy)
  'R' -> turn2 n (wx, wy, sx, sy)

run :: IO ()
run = do
  content <- readFile "../day12.txt"
  let x = map readOp (lines content)

  let p1 = foldl apply (0, 0, 1) x
  print (mdist p1)

  let p2 = foldl apply2d (10, 1, 0, 0) x
  print (mdist2 p2)