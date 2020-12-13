#!/usr/bin/env runhaskell

import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Math.NumberTheory.Moduli.Chinese (chinese)
import Text.Read (readMaybe)

findSolution :: [(Integer,Integer)] -> Integer
findSolution (x:xs) = uncurry mod $ go x xs
  where
    go m [] = m
    go m (n:ns) = go (fromJust (chinese m n), snd m * snd n) ns

parseInput :: [String] -> [(Integer,Integer)]
parseInput [_,buses] = map busOffsetDifference . filter busInService $ zip [0..] (map readMaybeBus (splitOn "," buses))
  where
    readMaybeBus str =
      case readMaybe str of
        Just int -> int
        Nothing  -> -1
    busInService (_,bus) = bus /= -1
    busOffsetDifference (offset,bus) = (bus - offset, bus)

main :: IO ()
main = interact $ show . findSolution . parseInput . lines
