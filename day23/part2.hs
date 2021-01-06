#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}

import Data.Char (digitToInt)
import Data.IntMap.Strict (IntMap, (!))

import qualified Data.IntMap.Strict as M

type Cup = Int

findDestinationCup :: Cup -> [Cup] -> Cup
findDestinationCup currentCup cupsToMove
  | currentCup `elem` cupsToMove = findDestinationCup (currentCup-1) cupsToMove
  | currentCup < 1 = findDestinationCup 1000000 cupsToMove
  | otherwise = currentCup

moveCups :: Cup -> Cup -> Cup -> Cup -> Cup -> Cup -> IntMap Cup -> IntMap Cup
moveCups srcStart srcEnd destStart destEnd firstCupToMove lastCupToMove =
  M.insert lastCupToMove destEnd . M.insert destStart firstCupToMove . M.insert srcStart srcEnd

doMove :: (Cup,IntMap Cup) -> (Cup,IntMap Cup)
doMove (currentCup,cups) = (nextCup, moveCups currentCup nextCup destinationCup nextCup' cupToMove1 cupToMove3 cups)
  where
    cupToMove1 = cups ! currentCup
    cupToMove2 = cups ! cupToMove1
    cupToMove3 = cups ! cupToMove2
    nextCup = cups ! cupToMove3
    destinationCup = findDestinationCup (currentCup-1) [cupToMove1,cupToMove2,cupToMove3]
    nextCup' = cups ! destinationCup

extendCups :: [Cup] -> (Cup,IntMap Cup)
extendCups cups =
  let extendedCups = cups ++ [(maximum cups+1)..1000000]
  in (head cups, M.fromList $ zip extendedCups (tail (cycle extendedCups)))

moveNTimes :: Int -> (Cup,IntMap Cup) -> IntMap Cup
moveNTimes 0 (_,cups) = cups
moveNTimes n eCups = moveNTimes (n-1) (doMove eCups)

takeFinalCups :: IntMap Cup -> Int
takeFinalCups cups =
  let after1 = cups ! 1
      afterAfter1 = cups ! after1
  in after1 * afterAfter1

main :: IO ()
main = interact $ show . takeFinalCups . moveNTimes 10000000 . extendCups . map digitToInt . init
