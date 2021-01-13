#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base, containers
-}

import Data.Map.Strict (Map, (!))
import Data.Maybe (mapMaybe)

import qualified Data.Map.Strict as M

type Direction = (Int,Int)
type Position = (Int,Int)
type Seat = Char
type Seats = Map Position Seat
type AugmentedSeats = (Int,Int,Seats)

isFloor :: Seat -> Bool
isFloor '.' = True
isFloor _   = False

isEmpty :: Seat -> Bool
isEmpty 'L' = True
isEmpty _   = False

isOccupied :: Seat -> Bool
isOccupied '#' = True
isOccupied _   = False

getSeat :: AugmentedSeats -> Position -> Seat
getSeat (_,_,seats) = (seats !)

getPossibleDirections :: Int -> Int -> Position -> [Position]
getPossibleDirections w h (x,y) =
  [ (x' - x, y' - y)
  | x' <- [max 0 (x-1)..min (w-1) (x+1)]
  , y' <- [max 0 (y-1)..min (h-1) (y+1)]
  , not (x' == x && y' == y)
  ]

hasReachedEdge :: Int -> Int -> Position -> Direction -> Bool
hasReachedEdge w h (x,y) (dx,dy) = (x == 0 && dx == -1)
  || (x == (w-1) && dx == 1)
  || (y == 0 && dy == -1)
  || (y == (h-1) && dy == 1)

exploreDirection :: AugmentedSeats -> Position -> Direction -> Maybe Position
exploreDirection aseats@(w,h,_) (x,y) dir@(dx,dy) = go (x+dx, y+dy)
  where
    go pos'@(x',y')
      | not (isFloor (getSeat aseats pos')) = Just pos'
      | hasReachedEdge w h pos' dir = Nothing
      | otherwise = go (x'+dx, y'+dy)

getVisibleSeats :: AugmentedSeats -> Position -> [Seat]
getVisibleSeats aseats@(w,h,_) pos =
  let possibleDirections = getPossibleDirections w h pos
  in map (getSeat aseats) (mapMaybe (exploreDirection aseats pos) possibleDirections)

willBecomeEmpty :: AugmentedSeats -> Position -> Bool
willBecomeEmpty aseats pos = isOccupied (getSeat aseats pos)
  && length (filter isOccupied (getVisibleSeats aseats pos)) >= 5

willBecomeOccupied :: AugmentedSeats -> Position -> Bool
willBecomeOccupied aseats pos = isEmpty (getSeat aseats pos)
  && not (any isOccupied (getVisibleSeats aseats pos))

computeNextStateForSeat :: AugmentedSeats -> Position -> Seat -> Seat
computeNextStateForSeat aseats pos seat
  | willBecomeEmpty aseats pos    = 'L'
  | willBecomeOccupied aseats pos = '#'
  | otherwise                     = seat

computeNextState :: AugmentedSeats -> AugmentedSeats
computeNextState aseats@(w,h,seats) = (w, h, M.mapWithKey (computeNextStateForSeat aseats) seats)

convergeSeats :: (AugmentedSeats -> AugmentedSeats) -> AugmentedSeats -> AugmentedSeats
convergeSeats f seats =
  let seats' = f seats
  in if seats' == seats
    then seats
    else convergeSeats f seats'

buildSeatGrid :: [[Seat]] -> AugmentedSeats
buildSeatGrid grid =
  let width = length $ head grid
      height = length grid
      seats = M.fromList $ concat $ zipWith (\j row -> zipWith (\i seat -> ((i,j), seat)) [0..] row) [0..] grid
  in (width, height, seats)

countOccupiedSeats :: AugmentedSeats -> Int
countOccupiedSeats = M.size . M.filter isOccupied . (\(_,_,seats) -> seats)

main :: IO ()
main = interact $ show . countOccupiedSeats . convergeSeats computeNextState . buildSeatGrid . lines
