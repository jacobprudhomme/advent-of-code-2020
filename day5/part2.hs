#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Data.Bifunctor (bimap)
import Data.List (sort)

splitRowAndColumn :: String -> (String,String)
splitRowAndColumn = splitAt 7

determineRowOrColumn :: String -> Int
determineRowOrColumn = foldl
  (\position section ->
    if section == 'F' || section == 'L'
    then position * 2
    else (position * 2) + 1)
  0

getSeatPosition :: (String,String) -> (Int,Int)
getSeatPosition = bimap determineRowOrColumn determineRowOrColumn

getSeatID :: (Int,Int) -> Int
getSeatID (row,column) = row * 8 + column

findFirstNonconsecutiveSeat :: [Int] -> Int
findFirstNonconsecutiveSeat [] = 864
findFirstNonconsecutiveSeat (s1:s2:ss)
  | s2 /= succ s1 = succ s1
  | otherwise     = findFirstNonconsecutiveSeat (s2:ss)

main :: IO ()
main = interact $ show . findFirstNonconsecutiveSeat . sort . map (getSeatID . getSeatPosition . splitRowAndColumn) . lines
