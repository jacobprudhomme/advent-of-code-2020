#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Data.Bifunctor (bimap)

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

main :: IO ()
main = interact $ show . maximum . map (getSeatID . getSeatPosition . splitRowAndColumn) . lines
