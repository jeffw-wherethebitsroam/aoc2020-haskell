module Day22
  ( run,
  )
where

import qualified Data.Set as Set

play as [] = (as, True)
play [] bs = (bs, False)
play (a : as) (b : bs) = do
  if a > b then play (as ++ [a, b]) bs else play as (bs ++ [b, a])

play2 as [] _ = (as, True)
play2 [] bs _ = (bs, False)
play2 as@(a : at) bs@(b : bt) games =
  if Set.member (as, bs) games
    then ([], True)
    else do
      let g = Set.insert (as, bs) games
      let p1win = if (length at >= a) && (length bt >= b) then snd (play2 (take a at) (take b bt) g) else a > b
      if p1win then play2 (at ++ [a, b]) bt g else play2 at (bt ++ [b, a]) g

run :: IO ()
run = do
  let p1 = [10, 39, 16, 32, 5, 46, 47, 45, 48, 26, 36, 27, 24, 37, 49, 25, 30, 13, 23, 1, 9, 3, 31, 14, 4]
  let p2 = [2, 15, 29, 41, 11, 21, 8, 44, 38, 19, 12, 20, 40, 17, 22, 35, 34, 42, 50, 6, 33, 7, 18, 28, 43]
  -- let p1 = [9, 2, 6, 3, 1]
  -- let p2 = [5, 8, 4, 7, 10]

  let (c, w) = play p1 p2
  let score = sum (zipWith (*) [1 ..] (reverse c))
  print score

  let (c2, w2) = play2 p1 p2 Set.empty
  let score2 = sum (zipWith (*) [1 ..] (reverse c2))
  print score2
