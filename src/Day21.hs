module Day21
  ( run,
  )
where

import Data.Bifunctor
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace
import qualified Text.Parsec as P

parseLine = do
  ingr <- P.endBy (P.many1 P.letter) (P.char ' ')
  P.string "(contains "
  allergens <- P.sepBy (P.many1 P.letter) (P.string ", ")
  P.char ')'
  return (ingr, allergens)

parseFile = P.endBy parseLine (P.char '\n')

addLine :: ([String], [String]) -> Map.Map String (Set.Set String) -> Map.Map String (Set.Set String)
addLine (ingrs, algns) m = do
  let set = Set.fromList ingrs
  foldr (addAlgn set) m algns

addAlgn :: Set.Set String -> String -> Map.Map String (Set.Set String) -> Map.Map String (Set.Set String)
addAlgn ingrs algn m = case Map.lookup algn m of
  Nothing -> Map.insert algn ingrs m
  Just x -> Map.insert algn (Set.intersection x ingrs) m

findAllergens :: [(String, [String])] -> [(String, String)]
findAllergens [] = []
findAllergens m = do
  let (f, r) = partition (\(_, is) -> length is == 1) m
  let found = map (second head) f
  let allFound = map snd found
  let rest = map (second (filter (`notElem` allFound))) r
  found ++ findAllergens rest

run :: IO ()
run = do
  content <- readFile "../day21.txt"

  let list = case P.parse parseFile "" content of
        Right x -> x
        Left x -> error ("parse error: " ++ show x)

  let s = foldr addLine Map.empty list
  -- get the list of possible allergens
  let excl = Map.foldr Set.union Set.empty s
  -- get all occurances on ingredients not in the allergens set
  let safe = foldr (\(ingrs, _) acc -> acc ++ filter (`Set.notMember` excl) ingrs) [] list
  print (length safe)

  let ai = findAllergens (Map.toList (Map.map Set.toList s))
  let sorted = sortBy (\(a1, _) (a2, _) -> compare a1 a2) ai
  print (intercalate "," (map snd sorted))
