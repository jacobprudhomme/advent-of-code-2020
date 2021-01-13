#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base
-}

type Line = (Int,Int,Char,String)

parseLine :: String -> Line
parseLine str = (read minStr, read maxStr, head letterStr, drop 2 rest3)
  where
    (minStr, rest1) = break (== '-') str
    (maxStr, rest2) = break (== ' ') (tail rest1)
    (letterStr, rest3) = break (== ':') (tail rest2)

countOccurrences :: Line -> (Int,Int,Int)
countOccurrences (minVal,maxVal,letter,password) = (minVal, maxVal, occurrences)
  where occurrences = length $ filter (== letter) password

isValid :: (Int,Int,Int) -> Bool
isValid (minVal,maxVal,occurrences) = minVal <= occurrences && occurrences <= maxVal

countValid :: [Line] -> Int
countValid = length . filter id . map (isValid . countOccurrences)

main :: IO ()
main = interact $ show . countValid . map parseLine . lines
