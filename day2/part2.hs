#!/usr/bin/env runhaskell

type Line = (Int,Int,Char,String)

parseLine :: String -> Line
parseLine str = (read minStr, read maxStr, head letterStr, drop 2 rest3)
  where
    (minStr, rest1) = break (== '-') str
    (maxStr, rest2) = break (== ' ') (tail rest1)
    (letterStr, rest3) = break (== ':') (tail rest2)

isValid :: Line -> Bool
isValid (pos1,pos2,letter,password) =
  (password !! (pos1 - 1) == letter) ^ (password !! (pos2 - 1) == letter)
    where x ^ y = x && not y || not x && y

countValid :: [Line] -> Int
countValid = length . filter id . map isValid

main :: IO ()
main = interact $ show . countValid . map parseLine . lines
