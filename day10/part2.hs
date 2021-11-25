import Data.IntMap.Strict (IntMap, (!))
import Data.List (sort)

import qualified Data.IntMap.Strict as M

collectPossibilities :: [Int] -> IntMap Int
collectPossibilities ns = foldr calculateNumberOfPossibilities initialMap (init nsWithIndices)
  where
    ns' = 0 : ns ++ [last ns + 3]
    nsWithIndices = zip [0..] ns'
    initialMap = M.fromList [(length ns' - 1, 1)]
    calculateNumberOfPossibilities (i,joltageRating) accMap =
      M.insert i (foldr sumPriorPossibilities 0 priorCalculated) accMap
        where
          priorCalculated = drop (i+1) nsWithIndices
          sumPriorPossibilities (j,joltageRating') accSum =
            if joltageRating' - joltageRating <= 3
            then (accMap ! j) + accSum
            else accSum

main :: IO ()
main = interact $ show . (! 0) . collectPossibilities . sort . map read . lines
