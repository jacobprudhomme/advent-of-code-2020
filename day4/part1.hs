import Data.List.Split (splitWhen)

getKeys :: [String] -> [String]
getKeys = map (takeWhile (/= ':'))

requiredKeys :: [String]
requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValidPassport :: [String] -> Bool
isValidPassport keys = all (`elem` keys) requiredKeys

countValid :: [[String]] -> Int
countValid = length . filter isValidPassport

main :: IO ()
main = interact $ show . countValid . map (getKeys . concatMap words) . splitWhen null . lines
