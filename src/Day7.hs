module Day7
  ( run,
  )
where

import Data.List
import Text.Parsec

bagCountParser :: Parsec String () (String, Int)
bagCountParser = do
  count <- many1 digit
  char ' '
  bag <- bagParser
  char ' '
  many1 letter -- bag or bags
  return (bag, read count)

bagParser :: Parsec String () String
bagParser = do
  desc <- many1 letter
  char ' '
  colour <- many1 letter
  return (desc ++ " " ++ colour)

parseEmpty :: Parsec String () [(String, Int)]
parseEmpty = do
  string "no other bags."
  return []

parseCounts :: Parsec String () [(String, Int)]
parseCounts = do
  bags <- sepBy bagCountParser (string ", ")
  char '.'
  return bags

rowParser :: Parsec String () (String, [(String, Int)])
rowParser = do
  bag <- bagParser
  string " bags contain "
  contains <- choice [parseEmpty, parseCounts]
  char '\n'
  return (bag, contains)

fileParser :: Parsec String () [(String, [(String, Int)])]
fileParser = many rowParser

trans = concatMap (\(b, cs) -> map (\c -> (fst c, b)) cs)

findy :: String -> [(String, String)] -> [String]
findy n bs = do
  let f = filter (\(c, _) -> c == n) bs
  let cs = map snd f
  nub (cs ++ concatMap (`findy` bs) cs)

countBags n bs = do
  case find (\(b, _) -> b == n) bs of
    Nothing -> error "bag not found"
    Just (_, cs) -> do
      sum (map (\(bag, count) -> count + count * countBags bag bs) cs)

run :: IO ()
run = do
  content <- readFile "../day7.txt"
  let bags = case parse fileParser "" content of
        Left _ -> error "parse error"
        Right x -> x

  let blah = trans bags

  let x = findy "shiny gold" blah
  print (length x)

  let x = countBags "shiny gold" bags
  print x