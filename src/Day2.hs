module Day2
  ( run,
  )
where

import Data.List.Split

data Row = Row {mn :: Int, mx :: Int, letter :: Char, str :: String} deriving (Show)

splitRow = splitOneOf "- :"

parseRow (a : b : c : _ : d : _) = do
  Row (read a) (read b) (head c) d

countLetters :: String -> Char -> Int
countLetters str c = length $ filter (== c) str

valid :: Row -> Bool
valid Row {mn = m1, mx = m2, str = s, letter = l} = do
  let c = countLetters s l
  c >= m1 && c <= m2

valid2 :: Row -> Bool
valid2 Row {mn = m1, mx = m2, str = s, letter = l} = do
  let p1 = s !! (m1 -1) == l
  let p2 = s !! (m2 -1) == l
  p1 /= p2

run :: IO ()
run = do
  content <- readFile "../day2.txt"
  let linesOfFile = lines content

  let y = map (parseRow . splitRow) linesOfFile
  print (head y)

  let z = filter valid y
  print (length z)

  let p2 = filter valid2 y
  print (length p2)
