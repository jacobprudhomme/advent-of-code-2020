#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base ^>= 4.14.1.0
             , split ^>= 0.2.3.4
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
