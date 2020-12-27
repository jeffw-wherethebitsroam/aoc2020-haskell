module Day23
  ( run,
  )
where

import Array
import Data.List
import Debug.Trace

play 0 cs = cs
-- play n (c : cs) = trace ("n=" ++ show n ++ ", (c :cs)=" ++ show (c, take 20 cs)) play (n -1) (ins c (splitAt 3 cs) ++ [c])
play n (c : cs) = play (n -1) (ins c (splitAt 3 cs) ++ [c])

ins :: Int -> ([Int], [Int]) -> [Int]
ins d (pu, rst) = case elemIndex d rst of
  Just i -> do
    let (x, y) = splitAt (i + 1) rst
    x ++ pu ++ y
  Nothing -> ins (dec d) (pu, rst)

dec d = if d == 1 then 9 else d - 1

res (c : cs) = if c == 1 then concatMap show cs else res (cs ++ [c])

toNexts cs = Array.array (1, length cs) (nexts (length cs) cs)

nexts 0 _ = []
nexts n (c : cs) = (c, head cs) : nexts (n -1) (cs ++ [c])

play2 :: Int -> Int -> Array Int Int -> (Int, Int)
play2 n x a =
  if n == 0
    then do
      let a1 = a ! 1
      let a2 = a ! a1
      (a1, a2)
    else do
      let pu1 = traceShow n (a ! x)
      let pu2 = a ! pu1
      let pu3 = a ! pu2
      let ap = a ! pu3
      let ins = getIns (x -1) [pu1, pu2, pu3] (length a)
      let ai = a ! ins
      let a' = a // [(x, ap), (ins, pu1), (pu3, ai)]
      play2 (n -1) ap a'

getIns x pu l = do
  let i = if x == 0 then l else x
  if i `elem` pu then getIns (i -1) pu l else i

run :: IO ()
run = do
  -- let input = [1, 3, 5, 4, 6, 8, 7, 2, 9]
  let input = [3, 8, 9, 1, 2, 5, 4, 6, 7]
  let final = play 100 input
  print (res final)

-- too slow. Did in rust
-- let i2 = toNexts (input ++ [10 .. 1000000])
-- let a = play2 10000000 (head input) i2
-- print a
