module Day18
  ( run,
  )
where

import Data.Char
import Debug.Trace

data Expression = Int Int | Infix Expression Char Expression

instance Show Expression where
  show (Int x) = show x
  show (Infix l o r) = "(" ++ show l ++ " " ++ show o ++ " " ++ show r ++ ")"

toTokens = filter (/= ' ')

precedence :: Char -> Int
precedence t = case t of
  '+' -> 2
  '*' -> 1
  _ -> 0

parsePrefix :: [Char] -> (Expression, [Char])
parsePrefix (t : ts)
  | isDigit t = (Int (digitToInt t), ts)
  | otherwise = case t of
    '(' -> do
      let (exp, x : xs) = parseExpression 0 ts
      if x /= ')' then error ("expected ')', got " ++ show x) else (exp, xs)
    x -> error ("bad prefix: " ++ [x])

parseInfix :: Expression -> [Char] -> (Expression, [Char])
parseInfix left (t : ts) = do
  let (right, ets) = parseExpression (precedence t) ts
  (Infix left t right, ets)

parseInfixes :: Int -> (Expression, [Char]) -> (Expression, [Char])
parseInfixes prec (left, []) = (left, [])
parseInfixes prec (left, ts) = do
  let p = precedence (head ts)
  if prec < p then parseInfixes prec (parseInfix left ts) else (left, ts)

parseExpression :: Int -> [Char] -> (Expression, [Char])
parseExpression prec ts = parseInfixes prec (parsePrefix ts)

eval :: Expression -> Int
eval e = case e of
  Int x -> x
  Infix l o r -> case o of
    '+' -> eval l + eval r
    '*' -> eval l * eval r

run :: IO ()
run = do
  content <- readFile "../day18.txt"
  let p1 = (sum . map (eval . fst . parseExpression 0 . toTokens)) (lines content)
  print p1