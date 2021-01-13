#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base
-}

shiftLinesBy :: (Int, Int) -> [String] -> [Char]
shiftLinesBy _     [] = []
shiftLinesBy (m,n) xs = head (head xs) : shiftLinesBy (m,n) (map (drop m) (drop n xs))

countTrees :: [Char] -> Int
countTrees = length . filter (== '#')

slopes :: [(Int,Int)]
slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

main :: IO ()
main = interact $ show . product . map countTrees . traverse shiftLinesBy slopes . map cycle . lines
