#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}

import Data.List (nub)
import Text.ParserCombinators.ReadP

type Coord = (Int,Int,Int)
data Direction = E | SE | SW | W | NW | NE

directionToCoordinate :: Direction -> Coord
directionToCoordinate E  = ( 1,  1,  0)
directionToCoordinate W  = (-1, -1,  0)
directionToCoordinate NE = ( 1,  0,  1)
directionToCoordinate NW = ( 0, -1,  1)
directionToCoordinate SE = ( 0,  1, -1)
directionToCoordinate SW = (-1,  0, -1)

addCoordinates :: [Coord] -> Coord
addCoordinates = foldr (\(x,y,z) (xAcc,yAcc,zAcc) -> (x+xAcc, y+yAcc, z+zAcc)) (0,0,0)

findTileLocation :: [Direction] -> Coord
findTileLocation = addCoordinates . map directionToCoordinate

parsePathToTile :: ReadP [Direction]
parsePathToTile = many1 $
      (E  <$ char   'e')
  +++ (SE <$ string "se")
  +++ (SW <$ string "sw")
  +++ (W  <$ char 'w')
  +++ (NW <$ string "nw")
  +++ (NE <$ string "ne")

countOccurrences :: Eq a => [a] -> [(a,Int)]
countOccurrences xs = [ (x, length (filter (==x) xs)) | x <- nub xs ]

countBlackTiles :: [Coord] -> Int
countBlackTiles = length . filter (odd . snd) . countOccurrences

main :: IO ()
main = interact $ show . countBlackTiles . map (findTileLocation . fst . last . readP_to_S parsePathToTile) . lines
