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

-- returns the hash and whether it was reversed
lowestHash s = do
  let h1 = hashEdge s
  let h2 = hashEdge (reverse s)
  if h1 < h2 then h1 else h2

-- ..##.#..#.
hashEdge = binToInt . map (\x -> if x == '#' then 1 else 0)

binToInt :: [Int] -> Int
binToInt = foldl' (\acc x -> acc * 2 + x) 0

goupCount = map (\xs@(x : _) -> (x, length xs)) . group . sort

toScores cnts = map (\e -> snd (head (filter (\x -> e == fst x) cnts)))

findDim n x = if x * x == n then x else findDim n (x + 1)

findDim1 x = findDim x 1

arrange dim tinfo acc [] = acc
arrange dim tinfo acc (i : is) = do
  let (shp, hs) = constraints dim acc i
  let choice = head (filter (\ti -> (shape ti == sum shp) && matchesHashes hs ti) tinfo)

  -- transpose/reverse to match the expected shape and hashes
  let final = transposeMatch 8 hs shp choice

  let rest = filter (\ti -> tiId ti /= tiId final) tinfo

  arrange dim rest (acc ++ [(i, final)]) is

matchHash :: [Maybe Int] -> [Int] -> Bool
matchHash mhs hs = all (uncurry matchMaybe) (zip mhs hs)

matchMaybe :: Maybe Int -> Int -> Bool
matchMaybe m i = case m of
  Nothing -> True
  Just x -> x == i

match mhs shp ti = tiShape ti == shp && matchHash mhs (tiHash ti)

transposeMatch 0 _ _ _ = error "bad match"
transposeMatch n mhs shp ti = if match mhs shp ti then ti else reverseMatch (n -1) mhs shp (transposeTile ti)

reverseMatch 0 _ _ _ = error "bad match"
reverseMatch n mhs shp ti = if match mhs shp ti then ti else transposeMatch (n -1) mhs shp (reverseTile ti)

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

findSeaMonsters :: Array.Array (Int, Int) Char -> [(Int, Int)] -> [(Int, Int)] -> Int
findSeaMonsters ary sm [] = 0
findSeaMonsters ary sm (i : is) = do
  let sma = smAt i sm
  let x = if all (\i -> ary ! i == '#') sma then 1 else 0
  x + findSeaMonsters ary sm is

smAt :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
smAt (x, y) = map (\(a, b) -> (a + x, b + y))

run :: IO ()
run = do
  content <- readFile "../day20.txt"
  let tiles = case P.parse parseFile "" content of
        Right x -> x
        Left x -> error ("parse error: " ++ show x)

  let edges = map (second tileEdges) tiles

  -- get the counts for each edge hash. This gives the number of matches
  let counts = goupCount (concatMap snd edges)
  let edgeInfo = map (\(id, hs) -> (id, hs, toScores counts hs)) edges

  -- corners will have a score of 6
  let corners = map (\(id, _, _) -> id) (filter (\(_, _, x) -> sum x == 6) edgeInfo)
  print (product corners)

  let tileInfo = zipWith (curry (\((id, tile), (_, hs, mc)) -> TileInfo id tile hs mc)) tiles edgeInfo

  let dim = findDim1 (length tiles)
  let bnds = range ((0, 0), (dim -1, dim -1))
  -- print bnds

  let xs = arrange dim tileInfo [] bnds

  -- make the tiles
  let ts = map (\(_, ti) -> tiTile ti) xs
  putStrLn " "
  putStrLn (printTiles dim ts)
  putStrLn " "
  let r1 = toRows dim (map trimTile ts)
  putStrLn (intercalate "\n" r1)

  let l = length r1

  --  01234567890123456789
  -- 0                  #
  -- 1#    ##    ##    ###
  -- 2 #  #  #  #  #  #
  let seaMonster = [(0, 18), (1, 0), (1, 5), (1, 6), (1, 11), (1, 12), (1, 17), (1, 18), (1, 19), (2, 1), (2, 4), (2, 7), (2, 10), (2, 13), (2, 16)]

  let smx = 15

  let count = sum (map (length . filter (== '#')) r1)
  print count

  let x1 = findSeaMonsters (toArray r1) seaMonster (range ((0, 0), (l -3, l -20)))
  print x1
  print (count - x1 * smx)
  let r2 = transpose r1
  let x2 = findSeaMonsters (toArray r2) seaMonster (range ((0, 0), (l -3, l -20)))
  print x2
  print (count - x2 * smx)
  let r3 = reverse r2
  let x3 = findSeaMonsters (toArray r3) seaMonster (range ((0, 0), (l -3, l -20)))
  print x3
  print (count - x3 * smx)
  let r4 = transpose r3
  let x4 = findSeaMonsters (toArray r4) seaMonster (range ((0, 0), (l -3, l -20)))
  print x4
  print (count - x4 * smx)
