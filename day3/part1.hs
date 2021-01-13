#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base ^>= 4.14.1.0
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
