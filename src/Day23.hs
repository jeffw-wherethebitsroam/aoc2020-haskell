module Day23
  ( run,
  )
where

import Array
import Control.Monad.State
import Data.Array.ST

-- (current, ary)
type GameState = (Int, Array.Array Int Int)

playGame :: Int -> Int -> State GameState (Int, Int)
playGame _ 0 = do
  (_, cups) <- get
  let a1 = cups ! 1
  let a2 = cups ! a1
  return (a1, a2)
playGame size n = do
  (current, cups) <- get
  let pu1 = cups ! current
  let pu2 = cups ! pu1
  let pu3 = cups ! pu2
  let ap = cups ! pu3
  let ins = getIns (current - 1) [pu1, pu2, pu3] size
  let ai = cups ! ins

  let cups' = cups // [(current, ap), (ins, pu1), (pu3, ai)]

  put (ap, cups)
  playGame size (n - 1)

playGame2 :: Int -> Int -> Int -> Array.Array Int Int -> (Int, Int)
playGame2 size n curr arr = runSTUArray $ do
  stuArr <- thaw arr
  return play2 size n stuArr

play2 :: Int -> Int -> Int -> STUArray s Int Int -> (Int, Int)
play2 _ 0 _ cups = do
  a1 <- readArray cups 1
  a2 <- readArray cups a1
  (a1, a2)
play2 size n curr cups = do
  pu1 <- readArray cups curr
  pu2 <- readArray cups pu1
  pu3 <- readArray cups pu2
  ap <- readArray cups pu3

  let ins = getIns (curr - 1) [pu1, pu2, pu3] size
  ai <- readArray cups ins

  writeArray cups curr ap
  writeArray cups ins pu1
  writeArray cups pu3 ai

  play2 size (n - 1) ap cups

toNexts cs = Array.array (1, length cs) (nexts (length cs) cs)

nexts 0 _ = []
nexts n (c : cs) = (c, head cs) : nexts (n -1) (cs ++ [c])

getIns x pu l = do
  let i = if x == 0 then l else x
  if i `elem` pu then getIns (i -1) pu l else i

run :: IO ()
run = do
  -- let input = [1, 3, 5, 4, 6, 8, 7, 2, 9]
  let input = [3, 8, 9, 1, 2, 5, 4, 6, 7]
  let ns = toNexts input
  print ns

  let final = execState (playGame (length input) 100) (3, ns)
  print final

  let i2 = toNexts (input ++ [10 .. 1000000])
  let a = evalState (playGame (length i2) 10000000) (head input, i2)
  print a
