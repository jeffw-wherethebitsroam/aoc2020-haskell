module Day20
  ( run,
  )
where

import Array
import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import qualified Text.Parsec as P

-- id, lines, hash (lowest,clockwise), matches
data TileInfo = TileInfo {tiId :: Int, tiTile :: [[Char]], tiHash :: [Int], tiShape :: [Int]} deriving (Show)

rev is = [is !! 2, is !! 1, head is, is !! 3]

reverseTile :: TileInfo -> TileInfo
reverseTile ti = TileInfo (tiId ti) (reverse (tiTile ti)) (rev (tiHash ti)) (rev (tiShape ti))

transposeTile :: TileInfo -> TileInfo
transposeTile ti = TileInfo (tiId ti) (transpose (tiTile ti)) (reverse (tiHash ti)) (reverse (tiShape ti))

printTile :: TileInfo -> String
printTile ti = do
  let tile = tiTile ti
  intercalate "\n" tile

trimTile :: [[Char]] -> [[Char]]
trimTile tile = do
  let trimRows = map (take 8 . drop 1) tile
  take 8 (drop 1 trimRows)

printTiles :: Int -> [[[Char]]] -> String
printTiles dim tiles = do
  let lines = chunksOf dim tiles
  let rowsRows = map transpose lines
  (intercalate "\n\n" . map (intercalate "\n" . map unwords)) rowsRows

toRows :: Int -> [[[Char]]] -> [[Char]]
toRows dim tiles = do
  let lines = chunksOf dim tiles
  let rowsRows = map transpose lines
  concatMap (map concat) rowsRows

toArray :: [[Char]] -> Array.Array (Int, Int) Char
toArray rows = do
  let l = length rows
  let enum = concat (zipWith (\i r -> zipWith (\j c -> ((i, j), c)) [0 ..] r) [0 ..] rows)
  Array.array ((0, 0), (l -1, l -1)) enum

parseTile :: P.Parsec String () (Int, [[Char]])
parseTile = do
  P.string "Tile "
  id <- P.many1 P.digit
  P.string ":\n"
  lines <- P.endBy (P.many1 (P.oneOf ".#")) (P.char '\n')
  return (read id, lines)

parseFile = P.sepBy parseTile (P.char '\n')

tileEdges s = do
  let t = lowestHash (head s)
  let b = lowestHash (last s)
  let s' = transpose s
  let l = lowestHash (head s')
  let r = lowestHash (last s')
  [t, r, b, l]

lowestHash s = minimum [hashEdge s, hashEdge (reverse s)]

-- ..##.#..#.
hashEdge = binToInt . map (\x -> if x == '#' then 1 else 0)

binToInt :: [Int] -> Int
binToInt = foldl' (\acc x -> acc * 2 + x) 0

groupCount = map (\xs@(x : _) -> (x, length xs)) . group . sort

toScores cnts = map (\e -> snd (head (filter (\x -> e == fst x) cnts)))

findDim n x = if x * x == n then x else findDim n (x + 1)

findDim1 x = findDim x 1

arrange dim tinfo acc [] = acc
arrange dim tinfo acc (i : is) = do
  let (shp, hs) = constraints dim acc i
  let choice = head (filter (\ti -> (shape ti == sum shp) && matchesHashes hs ti) tinfo)

  -- transpose/reverse to match the expected shape and hashes
  let finalx = transposeMatch hs shp choice
  let final = trace (show finalx ++ "\n" ++ printTile finalx) finalx

  let rest = filter (\ti -> tiId ti /= tiId final) tinfo

  arrange dim rest (acc ++ [(i, final)]) is

matchHash :: [Maybe Int] -> [Int] -> Bool
matchHash mhs hs = all (uncurry matchMaybe) (zip mhs hs)

matchMaybe :: Maybe Int -> Int -> Bool
matchMaybe m i = case m of
  Nothing -> True
  Just x -> x == i

match mhs shp ti = tiShape ti == shp && matchHash mhs (tiHash ti)

transposeMatch mhs shp ti = if match mhs shp ti then ti else reverseMatch mhs shp (transposeTile ti)

reverseMatch mhs shp ti = if match mhs shp ti then ti else transposeMatch mhs shp (reverseTile ti)

shape :: TileInfo -> Int
shape ti = sum (tiShape ti)

matchesHashes hs ti = all (`elem` tiHash ti) (catMaybes hs)

constraints dim acc (r, c) = do
  -- [t, r, b, l]
  let shp = [edgeScore dim (r -1), edgeScore dim (c + 1), edgeScore dim (r + 1), edgeScore dim (c -1)]
  let hs =
        [ neighbourHash acc 2 (r -1, c),
          neighbourHash acc 3 (r, c + 1),
          neighbourHash acc 0 (r + 1, c),
          neighbourHash acc 1 (r, c -1)
        ]
  (shp, hs)

edgeScore dim x = if x < 0 || x >= dim then 1 else 2

neighbourHash acc dir ix = case find (\x -> fst x == ix) acc of
  Nothing -> Nothing
  Just (_, ti) -> Just (tiHash ti !! dir)

--  01234567890123456789
-- 0                  #
-- 1#    ##    ##    ###
-- 2 #  #  #  #  #  #
seaMonster = [(0, 18), (1, 0), (1, 5), (1, 6), (1, 11), (1, 12), (1, 17), (1, 18), (1, 19), (2, 1), (2, 4), (2, 7), (2, 10), (2, 13), (2, 16)]

seaMonsterAt :: (Int, Int) -> [(Int, Int)]
seaMonsterAt (x, y) = map (\(a, b) -> (a + x, b + y)) seaMonster

-- (f . g . h) x => f(g(h(x)))
findSeaMonsters :: Int -> Array.Array (Int, Int) Char -> Int
findSeaMonsters size ary = (length . filter id . map isSeaMonster) idxs
  where
    idxs = range ((0, 0), (size - 3, size - 20))
    isSeaMonster idx = all (\j -> ary ! j == '#') (seaMonsterAt idx)

run :: IO ()
run = do
  content <- readFile "../day20-test.txt"
  let tiles = case P.parse parseFile "" content of
        Right x -> x
        Left x -> error ("parse error: " ++ show x)

  let edges = map (second tileEdges) tiles

  -- get the counts for each edge hash. This gives the number of matches
  let counts = groupCount (concatMap snd edges)
  let edgeInfo = map (\(id, hs) -> (id, hs, toScores counts hs)) edges

  -- corners will have a score of 6
  let corners = map (\(id, _, _) -> id) (filter (\(_, _, x) -> sum x == 6) edgeInfo)
  print (product corners)

  -- part 2

  let tileInfo = zipWith (curry (\((id, tile), (_, hs, mc)) -> TileInfo id tile hs mc)) tiles edgeInfo
  let dim = findDim1 (length tiles)
  let bnds = range ((0, 0), (dim -1, dim -1))
  let xs = arrange dim tileInfo [] bnds

  -- make the tiles
  let ts = map (\(_, ti) -> tiTile ti) xs
  let r1 = toRows dim (map trimTile ts)
  let l = length r1

  let smx = 15

  let count = sum (map (length . filter (== '#')) r1)
  print count

  print (bounds (toArray r1))

  let x1 = findSeaMonsters l (toArray r1)
  print x1
  print (count - x1 * smx)
  let r2 = transpose r1
  let x2 = findSeaMonsters l (toArray r2)
  print x2
  print (count - x2 * smx)
  let r3 = reverse r2
  let x3 = findSeaMonsters l (toArray r3)
  print x3
  print (count - x3 * smx)
  let r4 = transpose r3
  let x4 = findSeaMonsters l (toArray r4)
  print x4
  print (count - x4 * smx)