module Day4
  ( run,
  )
where

import Data.Char
import Data.List.Split

hasKey k = any (\x -> head x == k)

hasRquiredKeys s = all (`hasKey` s) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

parsePassport :: String -> [[String]]
parsePassport = map (splitOn ":") . splitOneOf " \n"

validDate min max s = do
  let date = (read :: String -> Int) s
  date >= min && date <= max

validHgt s = do
  -- wtf :)
  let p = split (dropFinalBlank $ condense $ whenElt isLetter) s
  length p == 2 && case p !! 1 of
    "cm" -> validHgt' 150 193 (head p)
    "in" -> validHgt' 59 76 (head p)
    _ -> False

validHgt' min max s = do
  let h = (read :: String -> Int) s
  h >= min && h <= max

validHcl (a : as) = a == '#' && length as == 6 && all isHexDigit as

validEcl s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPid s = length s == 9 && all isDigit s

validator f = case f of
  "byr" -> validDate 1920 2002
  "iyr" -> validDate 2010 2020
  "eyr" -> validDate 2020 2030
  "hgt" -> validHgt
  "hcl" -> validHcl
  "ecl" -> validEcl
  "pid" -> validPid
  _ -> const True

valid = all (\p -> validator (head p) (p !! 1))

run :: IO ()
run = do
  content <- readFile "../day4.txt"
  let passports = map parsePassport (splitOn "\n\n" content)
  print (head passports)

  let v1 = filter hasRquiredKeys passports
  print (length v1)

  let v2 = filter valid v1
  print (length v2)