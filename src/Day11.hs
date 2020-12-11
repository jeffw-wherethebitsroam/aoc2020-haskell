module Day11
  ( run,
  )
where

import Array

type State = Array (Int, Int) Char

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0 ..]

dirs = filter (/= (0, 0)) (range ((-1, -1), (1, 1)))

add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

surrounding :: (Int, Int) -> State -> Int
surrounding ix seats = do
  let checks = filter (within (bounds seats)) (map (add ix) dirs)
  length (filter (\i -> seats ! i == '#') checks)

surrounding2 :: (Int, Int) -> State -> Int
surrounding2 ix seats = do
  let occ = map (occupied seats ix) dirs
  length (filter id occ)

occupied :: State -> (Int, Int) -> (Int, Int) -> Bool
occupied seats ix dir = do
  let ix' = add ix dir
  within (bounds seats) ix' && case seats ! ix' of
    '.' -> occupied seats ix' dir
    'L' -> False
    '#' -> True

within ((x1, y1), (x2, y2)) (x, y) = x >= x1 && x <= x2 && y >= y1 && y <= y2

newStates :: ((Int, Int) -> State -> Int) -> Int -> State -> State
newStates f c seats = do
  seats // [(ix, newState f c seats ix) | ix <- range (bounds seats)]

newState :: ((Int, Int) -> State -> Int) -> Int -> State -> (Int, Int) -> Char
newState f c seats ix =
  case seats ! ix of
    '.' -> '.'
    'L' -> if f ix seats == 0 then '#' else 'L'
    '#' -> if f ix seats >= c then 'L' else '#'

display :: State -> String
display s = display' s (range (bounds s))

display' :: State -> [(Int, Int)] -> String
display' _ [] = []
display' s (ix : ixs) = do
  let c = s ! ix
  let cs = if null ixs || fst (head ixs) /= fst ix then [c, '\n'] else [c]
  cs ++ display' s ixs

stable :: (State -> State) -> State -> State
stable ns curr = stable' ns curr (ns curr)

stable' :: (State -> State) -> State -> State -> State
stable' ns prev xcurr = do
  let curr = xcurr
  if prev == curr then curr else stable' ns curr (ns curr)

mapRow (i, str) = map (\(j, c) -> ((i, j), c)) (enumerate str)

mkArray :: [String] -> State
mkArray ss = do
  let rows = length ss
  let cols = length (head ss)
  array ((0, 0), (rows -1, cols -1)) (concatMap mapRow (enumerate ss))

occupiedCount s = length (filter (\ix -> s ! ix == '#') (range (bounds s)))

run :: IO ()
run = do
  content <- readFile "../day11.txt"
  let x = mkArray (lines content)
  putStrLn (display x)

  let ns1 = newStates surrounding 4

  let s = stable ns1 x
  putStrLn (display s)
  print (occupiedCount s)

  let ns2 = newStates surrounding2 5

  let s2 = stable ns2 x
  putStrLn (display s2)
  print (occupiedCount s2)