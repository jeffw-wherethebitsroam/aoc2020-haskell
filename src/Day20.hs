module Day20
  ( run,
  )
where

import Array
import Data.Bifunctor
import Data.List
import Data.Maybe
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

printTiles2 :: [((Int, Int), TileInfo)] -> String
printTiles2 tis = do
  let tiles = map (\(_, ti) -> tiTile ti) tis
  let zt = zip (head tiles) (tiles !! 1)
  let blah = map (\(t1, t2) -> t1 ++ " " ++ t2) zt
  intercalate "\n" blah

printTiles3 :: [((Int, Int), TileInfo)] -> String
printTiles3 tis = do
  let tiles = map (\(_, ti) -> tiTile ti) tis
  let zt = zip3 (head tiles) (tiles !! 1) (tiles !! 2)
  let blah = map (\(t1, t2, t3) -> t1 ++ " " ++ t2 ++ " " ++ t3) zt
  intercalate "\n" blah

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

-- arrange :: Int -> [TileInfo] -> [((Int, Int), TileInfo)] -> [(Int, Int)] -> ([Int], [Maybe Int])
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

run :: IO ()
run = do
  content <- readFile "../day20-test.txt"
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
  print bnds

  let xs = arrange dim tileInfo [] [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)]
  print xs
  putStrLn " "
  putStrLn (printTiles3 (take 3 xs))
  putStrLn " "
  putStrLn (printTiles3 (take 3 (drop 3 xs)))
  putStrLn " "
  putStrLn (printTiles3 (take 3 (drop 6 xs)))
