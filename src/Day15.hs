module Day15
  ( run,
  )
where

run :: IO ()
run = do
  content <- readFile "../day15-test.txt"
  print content