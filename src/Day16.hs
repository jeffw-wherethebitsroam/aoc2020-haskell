module Day16
  ( run,
  )
where

import Data.Bifunctor
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
  return (name, [(read l1 :: Int, read h1 :: Int), (read l2 :: Int, read h2 :: Int)])

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

validCol :: [(Int, Int)] -> [Int] -> Bool
validCol = all . inRanges

matchingIndexes cols (name, rs) = do
  let m = filter (\(_, cs) -> validCol rs cs) (zip [0 ..] cols)
  (name, map fst m)

process :: [(String, [Int])] -> [(String, Int)]
process [] = []
process cols = do
  case find (\(n, cs) -> length cs == 1) cols of
    Nothing -> error ("blah: " ++ show cols)
    Just (name, cs) -> do
      let col = head cs
      -- filter out the found name and column id
      let x = map (second (filter (/= col))) (filter (\(n, _) -> n /= name) cols)
      (name, col) : process x

run :: IO ()
run = do
  content <- readFile "../day16.txt"
  let (rules, yours, nearby) = case P.parse parseFile "" content of
        Left _ -> error "parse error"
        Right x -> x

  let ranges = concatMap snd rules

  -- part 1
  let outside = filter (not . inRanges ranges) (concat nearby)
  print (sum outside)

  -- part 2
  let validTickets = filter (all (inRanges ranges)) (yours : nearby)
  let t = transpose validTickets
  let validCols = map (matchingIndexes t) rules
  let p2 = process validCols
  let a = map snd (filter (\(n, _) -> "departure" `isPrefixOf` n) p2)
  let mine = map (yours !!) a
  print (product mine)
