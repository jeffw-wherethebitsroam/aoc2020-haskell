module Day4_v2
  ( run,
  )
where

import Text.Parsec

fieldParser = do
  name <- many1 letter
  char ':'
  value <- many1 (noneOf [' ', '\n'])
  return (name, value)

ppParser = many $ do
  field <- fieldParser
  oneOf [' ', '\n']
  return field

fileParser = sepBy ppParser (char '\n')

run :: IO ()
run = do
  content <- readFile "../day4.txt"
  let x = parse fileParser "" content
  print x