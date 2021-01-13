#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base ^>= 4.14.1.0
-}

selectTermsThatSumTo2020 :: [Int] -> (Int,Int,Int)
selectTermsThatSumTo2020 xs = head termsThatSumTo2020
  where
    termsThatSumTo2020 =
      [ (a,b,c)
      | a <- xs
      , b <- xs
      , c <- xs
      , a + b + c == 2020
      ]

mult3 :: (Int,Int,Int) -> Int
mult3 (a,b,c) = a * b * c

main :: IO ()
main = interact $ show . mult3 . selectTermsThatSumTo2020 . map read . lines
