#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base, containers, split
-}

import Data.List.Split (splitWhen)
import Data.Set (Set)

import qualified Data.Set as S

collectYesesForGroup :: [String] -> Set Char
collectYesesForGroup (personsAnswers:otherPeoplesAnswers) =
  foldr (S.intersection . S.fromList) (S.fromList personsAnswers) otherPeoplesAnswers

main :: IO ()
main = interact $ show . sum . map (S.size . collectYesesForGroup) . splitWhen null . lines
