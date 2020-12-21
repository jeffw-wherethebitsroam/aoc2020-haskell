module Day19
  ( run,
  )
where

import Data.List.Split
import qualified Data.Map as Map

-- 103: 12 40 | 57 116 -> Or [[12,40], [57,116]]
data Rule = Or [[Int]] | Char Char deriving (Show)

parseRule :: [Char] -> Rule
parseRule s =
  if head s == '"'
    then Char (s !! 1)
    else Or (map (map read . splitOn " ") (splitOn " | " s))

parseLine :: [Char] -> (Int, Rule)
parseLine s = do
  let ss = splitOn ": " s
  (read (head ss), parseRule (ss !! 1))

valid :: Map.Map Int Rule -> [Char] -> [Rule] -> Bool
valid _ [] [] = True
valid _ cs [] = False -- rules empty with chars remaining
valid _ [] rs = False -- chars empty with rules remaining
valid m cs@(c : ct) (r : rs) = case r of
  Char x -> c == x && valid m ct rs
  Or subs -> any (\is -> valid m cs (map (getRule m) is ++ rs)) subs

getRule :: Map.Map Int Rule -> Int -> Rule
getRule m i = case Map.lookup i m of
  Just x -> x
  Nothing -> error ("missing rule: " ++ show i)

run :: IO ()
run = do
  content <- readFile "../day19.txt"
  let blah = splitOn "\n\n" content
  let rules = map parseLine (lines (head blah))
  let cases = lines (blah !! 1)

  let m1 = Map.fromList rules
  let r = getRule m1 0

  -- part 1
  let c = map (\c -> (c, valid m1 c [r])) cases
  print (length (filter snd c))

  -- part 2
  -- 8: 42 | 42 8
  -- 11: 42 31 | 42 11 31
  let m2 = Map.insert 11 (parseRule "42 31 | 42 11 31") (Map.insert 8 (parseRule "42 | 42 8") m1)

  let c = map (\c -> (c, valid m2 c [r])) cases
  print (length (filter snd c))
