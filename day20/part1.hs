#!/usr/bin/env runhaskell

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.List (find)
import Data.List.Split (splitWhen)
import Data.Maybe (isJust)

type ID = Int
type Tile = [[Char]]
type Side = [Char]

data Zipper a = Zipper [a] a [a]

toZipper :: [a] -> Zipper a
toZipper (x:xs) = Zipper [] x xs

possibleSides :: [Tile -> Side]
possibleSides = (.) <$> [id, reverse] <*>
  [ head
  , map last
  , last
  , map head
  ]

getPossibleSides :: Tile -> [Side]
getPossibleSides tile = map ($ tile) possibleSides

findCorners :: [(ID,Tile)] -> [(ID,Tile)]
findCorners = go . toZipper
  where
    go (Zipper st t []) =
      let sides = getPossibleSides (snd t)
          otherTiles = reverse st
          otherSides = concatMap (getPossibleSides . snd) otherTiles
          matchingSides = filter (isJust . flip find otherSides . (==)) sides
      in if length matchingSides == 4 then [t] else []
    go (Zipper st t ts@(tNext:tsNext)) =
      let sides = getPossibleSides (snd t)
          otherTiles = reverse st ++ ts
          otherSides = concatMap (getPossibleSides . snd) otherTiles
          matchingSides = filter (isJust . flip find otherSides . (==)) sides
      in if length matchingSides == 4
        then t : go (Zipper (t:st) tNext tsNext)
        else go (Zipper (t:st) tNext tsNext)

splitTileAndId :: [String] -> (ID,Tile)
splitTileAndId = (read . init . dropWhile (not . isDigit) . head) &&& tail

multiplyIds :: [(ID,Tile)] -> Int
multiplyIds = product . map fst

main :: IO ()
main = interact $ show . multiplyIds . findCorners . map splitTileAndId . splitWhen null . lines
