#!/usr/bin/env cabal
{- cabal:
build-depends: base, split
-}

import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

calculateTimeOfNextArrival :: Int -> Int -> Int
calculateTimeOfNextArrival startTime bus =
  let timeSinceLastArrived = startTime `mod` bus
      timeUntilNextArrival = (bus - timeSinceLastArrived) `mod` bus
  in startTime + timeUntilNextArrival

parseInput :: [String] -> (Int,[Int])
parseInput [startTime,buses] = (read startTime, mapMaybe readMaybe (splitOn "," buses))

createTuples :: (Int,[Int]) -> [(Int,Int,Int)]
createTuples (startTime,buses) =
  let nextArrivalTimes = map (calculateTimeOfNextArrival startTime) buses
  in zip3 (repeat startTime) nextArrivalTimes buses

findSmallestWaitingTime :: [(Int,Int,Int)] -> (Int,Int,Int)
findSmallestWaitingTime = minimumBy (\(_,arrivalTime1,_) (_,arrivalTime2,_) -> compare arrivalTime1 arrivalTime2)

busTimesMinutes :: (Int,Int,Int) -> Int
busTimesMinutes (startTime,arrivalTime,bus) = (arrivalTime - startTime) * bus

main :: IO ()
main = interact $ show . busTimesMinutes . findSmallestWaitingTime . createTuples . parseInput . lines
