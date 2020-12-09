module Day9
  ( run,
  )
where

sums :: [Int] -> [Int]
sums (p : ps) = map (+ p) ps ++ sums ps
sums [] = []

exec :: [Int] -> [Int] -> Int
exec (p : ps) (d : ds) = do
  if d `elem` sums (p : ps)
    then exec (ps ++ [d]) ds
    else d

findSum :: Int -> [Int] -> [Int]
findSum s (d : ds) = do
  case test s (d : ds) 1 of
    Just x -> x
    Nothing -> findSum s ds

test :: Int -> [Int] -> Int -> Maybe [Int]
test s ds spl = do
  let (xs, _) = splitAt spl ds
  let sm = sum xs
  if sm == s
    then Just xs
    else
      if sm < s
        then test s ds (spl + 1)
        else Nothing

run :: IO ()
run = do
  content <- readFile "../day9.txt"

  let x = map (\b -> read b :: Int) (lines content)

  let (p, d) = splitAt 0 x
  let p1 = exec p d
  print p1

  let p2 = findSum 731031916 x
  print p2
  print (minimum p2 + maximum p2)