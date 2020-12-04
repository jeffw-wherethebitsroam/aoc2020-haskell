module Day4
  ( run,
  )
where

import Data.Char
import Text.Parsec

fieldParser :: Parsec String () (String, String)
fieldParser = do
  name <- many1 letter
  char ':'
  value <- many1 (noneOf [' ', '\n'])
  return (name, value)

passportParser :: Parsec String () [(String, String)]
passportParser = many $ do
  field <- fieldParser
  oneOf [' ', '\n']
  return field

fileParser :: Parsec String () [[(String, String)]]
fileParser = sepBy passportParser (char '\n')

hgtParser :: Parsec String () (Int, String)
hgtParser = do
  num <- many1 digit
  unit <- many1 letter
  return (read num, unit)

hasKey key = any (\(k, _) -> k == key)

hasRequiredKeys s = all (`hasKey` s) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validDate min max s = inRange min max (read s)

validHgt :: String -> Bool
validHgt s = case parse hgtParser "" s of
  Left _ -> False
  Right (n, u) -> case u of
    "cm" -> inRange 150 193 n
    "in" -> inRange 59 76 n
    _ -> False

inRange min max h = h >= min && h <= max

validator :: String -> String -> Bool
validator f = case f of
  "byr" -> validDate 1920 2002
  "iyr" -> validDate 2010 2020
  "eyr" -> validDate 2020 2030
  "hgt" -> validHgt
  "hcl" -> \(a : as) -> a == '#' && length as == 6 && all isHexDigit as
  "ecl" -> \s -> s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  "pid" -> \s -> length s == 9 && all isDigit s
  _ -> const True

valid = all (uncurry validator)

run :: IO ()
run = do
  content <- readFile "../day4.txt"

  let passports = case parse fileParser "" content of
        Left _ -> error "parse error"
        Right p -> p

  let v1 = filter hasRequiredKeys passports
  print (length v1)

  let v2 = filter valid v1
  print (length v2)