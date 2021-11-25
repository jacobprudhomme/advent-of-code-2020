{-# LANGUAGE TupleSections #-}

import Control.Arrow ((&&&))
import Data.Bifunctor (first, second)
import Data.List (inits, tails)

hasTermsThatSumTo :: [Int] -> Int -> Bool
hasTermsThatSumTo ns n = (> 0) $ sum [ 1 | n1 <- ns, n2 <- ns, n1 + n2 == n ]

processListOnce :: Int -> [Int] -> (Int,Bool)
processListOnce currN = (currN,) . uncurry hasTermsThatSumTo . second head . splitAt 25 . take 26

processList :: [Int] -> [(Int,Bool)]
processList ns
  | length ns == 25 = []
  | otherwise       = processListOnce (ns !! 25) ns : processList (tail ns)

allSubsequences :: [Int] -> [[Int]]
allSubsequences = concatMap inits . tails

matchingSubsequence :: Int -> [Int] -> [Int]
matchingSubsequence n ns = head [ subseq | subseq <- allSubsequences ns, length subseq >= 2, sum subseq == n ]

minMaxSum :: [Int] -> Int
minMaxSum = uncurry (+) . (minimum &&& maximum)

main :: IO ()
main = interact $ show . minMaxSum . uncurry matchingSubsequence . first (fst . head . filter (not . snd) . processList) . (\x -> (x,x)) . map read . lines
