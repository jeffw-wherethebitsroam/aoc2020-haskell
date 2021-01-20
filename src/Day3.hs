module Day3
  ( run,
  )
where

parseRow :: String -> [Bool]
parseRow = cycle . map (== '#')

isTree :: (Int, Int) -> (Int, [Bool]) -> Bool
isTree (r, d) (row, trees) = row `mod` d == 0 && trees !! (r * row `div` d)

treeCount trees path = (length . filter id . map (isTree path)) trees

run :: IO ()
run = do
  content <- readFile "../day3.txt"
  let linesOfFile = lines content
  let trees = map parseRow linesOfFile

  let etrees = zip [0 ..] trees
  let paths = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  let counts = map (treeCount etrees) paths
  print (product counts)