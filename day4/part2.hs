#!/usr/bin/env runhaskell

import Data.List.Split (splitOn, splitWhen)
import Data.Map.Strict ((!))
import Text.Regex.TDFA ((=~))

import qualified Data.Map.Strict as Map

getKeysAndValues :: [String] -> Map.Map String String
getKeysAndValues = Map.fromList . map ((\[key,val] -> (key,val)) . splitOn ":")

requiredKeys :: [String]
requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

hasRequiredData :: Map.Map String String -> Bool
hasRequiredData passport = all (`elem` Map.keys passport) requiredKeys

isValidYearRange :: String -> Int -> Int -> Map.Map String String -> Bool
isValidYearRange key lo hi = (\x -> lo <= x && x <= hi) . read . (! key)

isValidHeight :: Map.Map String String -> Bool
isValidHeight passport =
  let height = passport ! "hgt"
      heightInt = read . reverse . drop 2 . reverse $ height
      heightUnit = reverse . take 2 . reverse $ height
  in if heightUnit == "cm"
     then 150 <= heightInt && heightInt <= 193
     else heightUnit == "in" && 59 <= heightInt && heightInt <= 76

isValidHairColour :: Map.Map String String -> Bool
isValidHairColour =
  let hexStringRegex = "^#[0-9a-f]{6}$"
  in (=~ hexStringRegex) . (! "hcl")

isValidEyeColour :: Map.Map String String -> Bool
isValidEyeColour =
  let colours = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  in (`elem` colours) . (! "ecl")

isValidPassportId :: Map.Map String String -> Bool
isValidPassportId =
  let digitsRegex = "^[0-9]{9}$"
  in (=~ digitsRegex) . (! "pid")

validations :: [Map.Map String String -> Bool]
validations =
  [ isValidYearRange "byr" 1920 2002
  , isValidYearRange "iyr" 2010 2020
  , isValidYearRange "eyr" 2020 2030
  , isValidHeight
  , isValidHairColour
  , isValidEyeColour
  , isValidPassportId
  ]

hasValidData :: Map.Map String String -> Bool
hasValidData = and . sequenceA validations

isValidPassport :: Map.Map String String -> Bool
isValidPassport passport = hasRequiredData passport && hasValidData passport

countValid :: [Map.Map String String] -> Int
countValid = length . filter isValidPassport

main :: IO ()
main = interact $ show . countValid . map (getKeysAndValues . concatMap words) . splitWhen null . lines
