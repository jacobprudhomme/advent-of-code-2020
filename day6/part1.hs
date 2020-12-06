#!/usr/bin/env runhaskell

import Data.List.Split (splitWhen)
import Data.Set (union)

import qualified Data.Set as Set

collectYesesForGroup :: [String] -> Set.Set Char
collectYesesForGroup = foldr (union . Set.fromList) Set.empty

main :: IO ()
main = interact $ show . sum . map (Set.size . collectYesesForGroup) . splitWhen null . lines
