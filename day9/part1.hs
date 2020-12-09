#!/usr/bin/env runhaskell

{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (second)

hasTermsThatSumTo :: [Int] -> Int -> Bool
hasTermsThatSumTo ns n = (> 0) $ sum [ 1 | n1 <- ns, n2 <- ns, n1 + n2 == n ]

processListOnce :: Int -> [Int] -> (Int,Bool)
processListOnce currN = (currN,) . uncurry hasTermsThatSumTo . second head . splitAt 25 . take 26

processList :: [Int] -> [(Int,Bool)]
processList ns
  | length ns == 25 = []
  | otherwise       = processListOnce (ns !! 25) ns : processList (tail ns)

main :: IO ()
main = interact $ show . fst . head . filter (not . snd) . processList . map read . lines
