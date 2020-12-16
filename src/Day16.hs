module Day16
  ( run,
  )
where

import Data.List
import qualified Text.Parsec as P

-- departure location: 45-309 or 320-962
parseRange = do
  name <- P.many1 (P.choice [P.letter, P.space])
  P.string ": "
  l1 <- P.many1 P.digit
  P.char '-'
  h1 <- P.many1 P.digit
  P.string " or "
  l2 <- P.many1 P.digit
  P.char '-'
  h2 <- P.many1 P.digit
  P.char '\n'
  return (name, (read l1 :: Int, read h1 :: Int), (read l2 :: Int, read h2 :: Int))

parseTicket = do
  fields <- P.sepBy (P.many1 P.digit) (P.char ',')
  P.char '\n'
  return (map (\x -> read x :: Int) fields)

parseFile = do
  ranges <- P.manyTill parseRange (P.char '\n')
  P.string "your ticket:\n"
  your <- parseTicket
  P.string "\nnearby tickets:\n"
  nearby <- P.many1 parseTicket
  return (ranges, your, nearby)

inRanges rs x = any (\(l, h) -> x >= l && x <= h) rs

notInRanges rs x = not (inRanges rs x)

validCol :: (Int, Int) -> (Int, Int) -> Int -> [[Int]] -> Bool
validCol r1 r2 i = all (\t -> inRanges [r1, r2] (t !! i))

process :: [(String, Int)] -> [(String, [Int])] -> [(String, Int)]
process out [] = out
process out cols = do
  case find (\(n, cs) -> length cs == 1) cols of
    Nothing -> error ("blah: " ++ show cols)
    Just (name, cs) -> do
      let col = head cs
      let x = map (\(n, c) -> (n, filter (/= col) c)) (filter (\(n, _) -> n /= name) cols)
      process ((name, col) : out) x

run :: IO ()
run = do
  content <- readFile "../day16.txt"
  let (r, y, n) = case P.parse parseFile "" content of
        Left _ -> error "parse error"
        Right x -> x

  let ranges = concatMap (\(n, r1, r2) -> [r1, r2]) r

  let outside = filter (notInRanges ranges) (concat n)
  print (sum outside)

  let cols = [0 .. (length y - 1)]

  let tickets = y : n
  let valid = filter (all (inRanges ranges)) tickets

  let validCols = map (\(n, r1, r2) -> (n, filter (\i -> validCol r1 r2 i valid) cols)) r
  -- print validCols

  let p2 = process [] validCols
  print p2

  let a = map snd (filter (\(n, _) -> "departure" `isPrefixOf` n) p2)
  let mine = map (y !!) a
  print (product mine)
