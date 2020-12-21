module Day20
  ( run,
  )
where

import Data.Bifunctor
import Data.List
import qualified Data.Map as Map
import qualified Text.Parsec as P

parseTile :: P.Parsec String () (Int, [[Char]])
parseTile = do
  P.string "Tile "
  id <- P.many1 P.digit
  P.string ":\n"
  lines <- P.endBy (P.many1 (P.oneOf ".#")) (P.char '\n')
  return (read id, lines)

parseFile = P.sepBy parseTile (P.char '\n')

tileEdges s = do
  let t = bestHash (head s)
  let b = bestHash (last s)
  let s' = transpose s
  let l = bestHash (head s')
  let r = bestHash (last s')
  [t, r, b, l]

-- returns the hash and whether it was reversed
bestHash s = do
  let h1 = hashEdge s
  let h2 = hashEdge (reverse s)
  if h1 < h2 then (h1, False) else (h2, True)

-- ..##.#..#.
hashEdge = binToInt . map (\x -> if x == '#' then 1 else 0)

binToInt = foldl' (\acc x -> acc * 2 + x) 0

goupCount = map (\xs@(x : _) -> (x, length xs)) . group . sort

toScores cnts = map (\e -> snd (head (filter (\x -> e == fst x) cnts)))

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

rotate1 = rotate 1

-- rotates a tile clockwise
rotateTile :: [[Char]] -> [[Char]]
rotateTile = transpose . reverse

run :: IO ()
run = do
  content <- readFile "../day20-test.txt"
  let tiles = case P.parse parseFile "" content of
        Right x -> x
        Left x -> error ("parse error: " ++ show x)

  let edges = map (second tileEdges) tiles

  -- get the counts for each edge hash. This gives the number of matches
  let counts = goupCount (concatMap (\(_, es) -> map fst es) edges)
  let edgeScores = map (\(id, es) -> (id, es, toScores counts (map fst es))) edges
  -- print edgeScores

  -- corners will have a score of 6
  let corners = map (\(id, _, _) -> id) (filter (\(_, _, x) -> sum x == 6) edgeScores)
  print (product corners)

  -- get the start tile
  let tile = head (filter (\(id, _) -> id == 1951) tiles)
  print (snd tile)
  print (rotateTile (snd tile))
