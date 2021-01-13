#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base ^>= 4.14.1.0
             , containers ^>= 0.6.2.1
-}

import Data.Map.Strict (Map, (!))

import qualified Data.Map.Strict as M

type Position = (Int,Int)
type Seat = Char
type Seats = Map Position Seat
type AugmentedSeats = (Int,Int,Seats)

isEmpty :: Seat -> Bool
isEmpty 'L' = True
isEmpty _   = False

isOccupied :: Seat -> Bool
isOccupied '#' = True
isOccupied _   = False

getSeat :: AugmentedSeats -> Position -> Seat
getSeat (_,_,seats) = (seats !)

getAdjacentPositions :: Int -> Int -> Position -> [Position]
getAdjacentPositions w h (x,y) =
  [ (x',y')
  | x' <- [max 0 (x-1)..min (w-1) (x+1)]
  , y' <- [max 0 (y-1)..min (h-1) (y+1)]
  , not (x' == x && y' == y)
  ]

getAdjacentSeats :: AugmentedSeats -> Position -> [Seat]
getAdjacentSeats aseats@(w,h,_) pos =
  let adjacentPositions = getAdjacentPositions w h pos
  in map (getSeat aseats) adjacentPositions

willBecomeEmpty :: AugmentedSeats -> Position -> Bool
willBecomeEmpty aseats pos = isOccupied (getSeat aseats pos)
  && length (filter isOccupied (getAdjacentSeats aseats pos)) >= 4

willBecomeOccupied :: AugmentedSeats -> Position -> Bool
willBecomeOccupied aseats pos = isEmpty (getSeat aseats pos)
  && not (any isOccupied (getAdjacentSeats aseats pos))

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
