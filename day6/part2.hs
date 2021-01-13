#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base ^>= 4.14.1.0
             , containers ^>= 0.6.2.1
             , split ^>= 0.2.3.4
-}

import Data.List.Split (splitWhen)
import Data.Set (Set)

import qualified Data.Set as S

collectYesesForGroup :: [String] -> Set Char
collectYesesForGroup (personsAnswers:otherPeoplesAnswers) =
  foldr (S.intersection . S.fromList) (S.fromList personsAnswers) otherPeoplesAnswers

main :: IO ()
main = interact $ show . sum . map (S.size . collectYesesForGroup) . splitWhen null . lines
