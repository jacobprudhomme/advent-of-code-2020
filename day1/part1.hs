#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base ^>= 4.14.1.0
-}

selectTermsThatSumTo2020 :: [Int] -> (Int,Int)
selectTermsThatSumTo2020 xs = head termsThatSumTo2020
  where termsThatSumTo2020 = [ (a,b) | a <- xs, b <- xs, a + b == 2020 ]

main :: IO ()
main = interact $ show . uncurry (*) . selectTermsThatSumTo2020 . map read . lines
