#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base ^>= 4.14.1.0
             , arithmoi ^>= 0.11.0.1
             , split ^>= 0.2.3.4
-}

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
