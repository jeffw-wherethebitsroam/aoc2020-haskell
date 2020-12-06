module Day5
  ( run,
  )
where

import Data.Bits
import Data.List

letterToInt :: Char -> Int
letterToInt l = case l of
  'F' -> 0
  'B' -> 1
  'R' -> 1
  'L' -> 0
  _ -> error "Bad letter"

parseSeat :: String -> (Int, Int)
parseSeat s = do
  let is = map letterToInt s
  let i = makeI is
  let row = i `shiftR` 3
  let col = i .&. 0x7
  (row, col)

makeI a = makeI' (length a - 1) a

makeI' x (a : as) = (a `shiftL` x) + makeI' (x -1) as
makeI' 0 _ = 0
makeI' _ [] = 0

seatId (r, c) = r * 8 + c

findMissing :: Int -> [Int] -> [Int]
findMissing exp (a : as) = if exp == a then findMissing (exp + 1) as else exp : findMissing (exp + 1) (a : as)
findMissing _ [] = []

run :: IO ()
run = do
  content <- readFile "../day5.txt"
  let linesOfFile = lines content
  let seats = map parseSeat linesOfFile
  let seatIds = map seatId seats

  let m = maximum seatIds
  print m

  let s = sort seatIds
  let missing = findMissing (head s) s
  print missing