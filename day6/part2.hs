#!/usr/bin/env runhaskell

import Data.List.Split (splitWhen)
import Data.Set (intersection)

import qualified Data.Set as Set

collectYesesForGroup :: [String] -> Set.Set Char
collectYesesForGroup (personsAnswers:otherPeoplesAnswers) =
  foldr (intersection . Set.fromList) (Set.fromList personsAnswers) otherPeoplesAnswers

main :: IO ()
main = interact $ show . sum . map (Set.size . collectYesesForGroup) . splitWhen null . lines
