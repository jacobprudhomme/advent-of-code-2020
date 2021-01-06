#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

shiftLines :: [String] -> [String]
shiftLines = go 1
  where
    go _ [] = []
    go n (x:xs) = take n x : go (n + 3) xs

countTrees :: [Char] -> Int
countTrees = length . filter (== '#')

main :: IO ()
main = interact $ show . countTrees . map last . shiftLines . map cycle . lines
