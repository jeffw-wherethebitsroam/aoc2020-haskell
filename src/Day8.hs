module Day8
  ( run,
  )
where

import Text.Parsec

data Exit = Loop Int | Term Int deriving (Show)

data Instr = Changed [(String, Int)] | Noop deriving (Show)

instnParser :: Parsec String () (String, Int)
instnParser = do
  op <- choice [string "nop", string "acc", string "jmp"]
  char ' '
  sign <- choice [char '+', char '-']
  num <- many1 digit
  char '\n'
  let n = case sign of
        '+' -> read num
        '-' -> negate (read num)
  return (op, n)

fileParser :: Parsec String () [(String, Int)]
fileParser = many instnParser

exec :: [(String, Int)] -> Int -> [Int] -> Int -> Exit
exec instns acc ran i = do
  if i `elem` ran
    then Loop acc
    else
      if i > (length instns - 1)
        then Term acc
        else case instns !! i of
          ("nop", _) -> exec instns acc (i : ran) (i + 1)
          ("acc", x) -> exec instns (acc + x) (i : ran) (i + 1)
          ("jmp", x) -> exec instns acc (i : ran) (i + x)

update instns i = do
  let (x, y : ys) = splitAt i instns
  case y of
    ("nop", a) -> Changed (x ++ ("jmp", a) : ys)
    ("jmp", a) -> Changed (x ++ ("nop", a) : ys)
    ("acc", _) -> Noop

testChange instns i = do
  case update instns i of
    Changed is -> case exec is 0 [] 0 of
      Loop _ -> testChange instns (i + 1)
      Term acc -> acc
    Noop -> testChange instns (i + 1)

run :: IO ()
run = do
  content <- readFile "../day8.txt"
  let instns = case parse fileParser "" content of
        Left _ -> error "parse error"
        Right x -> x

  -- print instns
  let p1 = exec instns 0 [] 0
  print p1

  print (testChange instns 0)