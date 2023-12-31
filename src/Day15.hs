module Day15
  ( run,
    calc,
  )
where

import Data.List (foldl')
import qualified Data.Map as Map

add m x i = do
  let r = case Map.lookup x m of
        Just x -> i - x
        Nothing -> 0
  (Map.insert x i m, r)

apply i n (m, x) = do
  if i == n
    then x
    else do
      let nxt = add m x i
      apply (i + 1) n nxt

calc idx = do
  let n = [16, 1, 0, 18, 12, 14, 19]
  let (m, r) = foldl' (\(m, _) (i, x) -> add m x i) (Map.empty, 0) (zip [1 ..] n)
  apply 8 idx (m, r)

run :: IO ()
run = do
  print (calc 2020)
  print (calc 30000000)
