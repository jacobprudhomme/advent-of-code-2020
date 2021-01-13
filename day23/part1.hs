#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base
-}

import Data.Char (digitToInt)
import Data.List (foldl1')

type Cup = Int

findDestinationCup :: Cup -> [Cup] -> Cup -> Cup -> Cup
findDestinationCup currentCup cupsToMove minCup maxCup
  | currentCup `elem` cupsToMove = findDestinationCup (currentCup-1) cupsToMove minCup maxCup
  | currentCup < minCup = findDestinationCup maxCup cupsToMove minCup maxCup
  | otherwise = currentCup

doMove :: [Cup] -> [Cup]
doMove cups@(currentCup:otherCups) = cupsToLeft ++ (cupToLeft : cupsToMove) ++ cupsToRight ++ [currentCup]
  where
    (cupsToMove,restOfCups) = splitAt 3 otherCups
    minCup = minimum cups
    maxCup = maximum cups
    destinationCup = findDestinationCup (currentCup-1) cupsToMove minCup maxCup
    (_:cupsToLeft,cupToLeft:cupsToRight) = break (==destinationCup) (currentCup:restOfCups)

takeFinalCups :: [Cup] -> Int
takeFinalCups cups =
  let finalCups = take (length cups-1) (tail (dropWhile (/= 1) (cycle cups)))
  in foldl1' (\acc cup -> acc * 10 + cup) finalCups

main :: IO ()
main = interact $ show . takeFinalCups . (!! 100) . iterate doMove . map digitToInt . init
