module Day14
  ( run,
  )
where

import Data.Bits
import Data.Char
import Data.List
import qualified Data.Map as Map
import qualified Text.Parsec as P

data Op = Mask (Int, Int) | Set (Int, Int) | Mask2 (Int, [Int]) deriving (Show)

-- (registers, mask)
type State = (Map.Map Int Int, (Int, Int))

type State2 = (Map.Map Int Int, (Int, [Int]))

parseOp f = do
  P.char 'm'
  P.choice [parseMask f, parseSet]

-- mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
parseMask :: ([Char] -> Op) -> P.Parsec String () Op
parseMask f = do
  P.string "ask = "
  mask <- P.many1 (P.oneOf "X01")
  P.char '\n'
  return (f mask)

-- mem[8] = 11
parseSet :: P.Parsec String () Op
parseSet = do
  P.string "em["
  loc <- P.many1 P.digit
  P.string "] = "
  val <- P.many1 P.digit
  P.char '\n'
  return (Set (read loc, read val))

fileParser :: ([Char] -> Op) -> P.Parsec String () [Op]
fileParser f = P.many (parseOp f)

readMask :: [Char] -> Op
readMask s = do
  let a = map (toInt 1) s
  let o = map (toInt 0) s
  Mask (binToInt a, binToInt o)

binToInt = foldl' (\acc x -> acc * 2 + x) 0

readMask2 :: [Char] -> Op
readMask2 s = do
  let o = binToInt (map (toInt 0) s)
  let f = mkFloating [] 1 (reverse s)
  Mask2 (o, f)

mkFloating :: [Int] -> Int -> [Char] -> [Int]
mkFloating acc _ [] = acc
mkFloating acc n (c : cs) = case c of
  'X' -> mkFloating (n : acc) (n * 2) cs
  _ -> mkFloating acc (n * 2) cs

applyMask2 :: (Int, [Int]) -> Int -> [Int]
applyMask2 (o, fs) v = foldl' (\acc f -> acc ++ map (`xor` f) acc) [v .|. o] fs

toInt :: Int -> Char -> Int
toInt x c = case c of
  'X' -> x
  _ -> digitToInt c

apply :: State -> Op -> State
apply (reg, (a, o)) op = case op of
  Mask m -> (reg, m)
  Set (x, y) -> do
    let v = y .&. a .|. o
    (Map.insert x v reg, (a, o))

apply2 :: State2 -> Op -> State2
apply2 (reg, mask) op = case op of
  Mask2 m -> (reg, m)
  Set (x, v) -> do
    let addrs = applyMask2 mask x
    let reg' = foldl' (\acc k -> Map.insert k v acc) reg addrs
    (reg', mask)

run :: IO ()
run = do
  content <- readFile "../day14.txt"

  let ops = case P.parse (fileParser readMask) "" content of
        Left _ -> error "parse error"
        Right x -> x

  let state = foldl' apply (Map.empty, (0, 0)) ops
  let p1 = Map.foldr (+) 0 (fst state)
  print p1

  let ops2 = case P.parse (fileParser readMask2) "" content of
        Left _ -> error "parse error"
        Right x -> x

  let state = foldl' apply2 (Map.empty, (0, [])) ops2
  let p2 = Map.foldr (+) 0 (fst state)
  print p2
