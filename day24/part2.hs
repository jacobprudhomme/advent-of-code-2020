#!/usr/bin/env runhaskell

import Data.Set (Set)
import Text.ParserCombinators.ReadP

import qualified Data.Set as S

type Coord = (Int,Int,Int)
type BlackTiles = Set Coord
data Direction = E | SE | SW | W | NW | NE deriving Enum

directionToCoordinate :: Direction -> Coord
directionToCoordinate E  = ( 1,  1,  0)
directionToCoordinate W  = (-1, -1,  0)
directionToCoordinate NE = ( 1,  0,  1)
directionToCoordinate NW = ( 0, -1,  1)
directionToCoordinate SE = ( 0,  1, -1)
directionToCoordinate SW = (-1,  0, -1)

addCoordinates :: Coord -> Coord -> Coord
addCoordinates (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

findAdjacentCoordinates :: Coord -> [Coord]
findAdjacentCoordinates coord = map (addCoordinates coord . directionToCoordinate) [E .. NE]

isBlack :: BlackTiles -> Coord -> Bool
isBlack = flip S.member

isWhite :: BlackTiles -> Coord -> Bool
isWhite = (not .) . isBlack

willFlipToBlack :: BlackTiles -> Coord -> Bool
willFlipToBlack blackTiles coord =
  let numOfAdjacentBlackTiles = length (filter (isBlack blackTiles) (findAdjacentCoordinates coord))
  in isWhite blackTiles coord && numOfAdjacentBlackTiles == 2

willFlipToWhite :: BlackTiles -> Coord -> Bool
willFlipToWhite blackTiles coord =
  let numOfAdjacentBlackTiles = length (filter (isBlack blackTiles) (findAdjacentCoordinates coord))
  in isBlack blackTiles coord && (numOfAdjacentBlackTiles == 0 || numOfAdjacentBlackTiles > 2)

willFlip :: BlackTiles -> Coord -> Bool
willFlip blackTiles coord = willFlipToBlack blackTiles coord || willFlipToWhite blackTiles coord

flipTile :: BlackTiles -> Coord -> BlackTiles
flipTile blackTiles coord =
  if coord `S.member` blackTiles
  then S.delete coord blackTiles
  else S.insert coord blackTiles

setBlackTilesForDay :: BlackTiles -> BlackTiles
setBlackTilesForDay blackTiles =
  let blackTilesAndNeighbours = foldr S.insert blackTiles (concatMap findAdjacentCoordinates blackTiles)
  in foldr
    (\coord newBlackTiles ->
      if willFlip blackTiles coord
      then flipTile newBlackTiles coord
      else newBlackTiles)
    blackTiles
    blackTilesAndNeighbours

parse :: ReadP a -> (String -> a)
parse parser = fst . last . readP_to_S parser

parsePathToTile :: ReadP [Direction]
parsePathToTile = many1 $
      (E  <$ char   'e')
  +++ (SE <$ string "se")
  +++ (SW <$ string "sw")
  +++ (W  <$ char   'w')
  +++ (NW <$ string "nw")
  +++ (NE <$ string "ne")

findTileLocation :: [Direction] -> Coord
findTileLocation = foldr (addCoordinates . directionToCoordinate) (0,0,0)

initializeBlackTiles :: [Coord] -> BlackTiles
initializeBlackTiles = foldr (flip flipTile) S.empty

getBlackTilesOnDay100 :: BlackTiles -> BlackTiles
getBlackTilesOnDay100 = go 100
  where
    go 0 = id
    go n = go (n-1) . setBlackTilesForDay

main :: IO ()
main = interact $ show . S.size . getBlackTilesOnDay100 . initializeBlackTiles . map (findTileLocation . parse parsePathToTile) . lines
