#!/usr/bin/env runhaskell

import Control.Arrow ((&&&))
import Data.IntMap.Strict (IntMap, (!?))
import Data.List (foldl', unfoldr)
import Data.List.Split (splitOn)

import qualified Data.IntMap.Strict as M

initializeSeenNumbers :: [Int] -> ([Int],IntMap Int)
initializeSeenNumbers = id &&& foldl' (\acc (turn,num) -> M.insert num turn acc) M.empty . zip [1..] . init

initializeTurnAndLastSeenNumber :: ([Int],IntMap Int) -> (Int,Int,IntMap Int)
initializeTurnAndLastSeenNumber (nums,seenNums) = (length nums, last nums, seenNums)

initializeGame :: [Int] -> (Int,Int,IntMap Int)
initializeGame = initializeTurnAndLastSeenNumber . initializeSeenNumbers

playGameUntil :: Int -> (Int,Int,IntMap Int) -> Int
playGameUntil turnToStop = last . unfoldr produceNextNums
  where
    produceNextNums (turn,lastNum,seenNums) =
      if turn == turnToStop + 1
      then Nothing
      else case seenNums !? lastNum of
        Just turnLastSeen -> Just (lastNum, (turn+1, turn-turnLastSeen, M.insert lastNum turn seenNums))
        Nothing           -> Just (lastNum, (turn+1, 0, M.insert lastNum turn seenNums))

main :: IO ()
main = interact $ show . playGameUntil 30000000 . initializeGame . map read . splitOn ","
