module Day1
  ( run,
  )
where

import Data.List

find3' :: (Int, [Int]) -> Maybe (Int, Int, Int)
find3' (_, []) = Nothing
find3' (a, b : bs) = do
  case find (\c -> a + b + c == 2020) bs of
    Just n -> Just (a, b, n)
    Nothing -> find3' (a, bs)

find3 :: [Int] -> Maybe (Int, Int, Int)
find3 [] = Nothing
find3 (a : as) =
  case find3' (a, as) of
    Just x -> Just x
    Nothing -> find3 as

find2 :: [Int] -> Maybe (Int, Int)
find2 (a : as) = do
  case find (\x -> x + a == 2020) as of
    Just n -> Just (a, n)
    Nothing -> find2 as
find2 [] = Nothing

run :: IO ()
run = do
  content <- readFile "../day1.txt"
  let i = map (read :: String -> Int) (lines content)

  case find2 i of
    Just (a, b) -> do
      print (a, b)
      print (a * b)
    Nothing -> print "Not found!"

  case find3 i of
    Just (a, b, c) -> do
      print (a, b, c)
      print (a * b * c)
    Nothing -> print "Not found!"