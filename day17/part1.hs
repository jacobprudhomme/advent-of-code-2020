#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base, containers
-}

import Data.Map.Strict (Map, (!))

import qualified Data.Map.Strict as M

type Position = (Int,Int,Int)
type Cube = Char
type Plane = [[Cube]]
type Grid = Map Position Cube

isActive :: Grid -> Position -> Bool
isActive grid pos
  | pos `M.member` grid && grid ! pos == '#' = True
  | otherwise = False

getAdjacentPositions :: Position -> [Position]
getAdjacentPositions (x,y,z) =
  [ (x',y',z')
  | x' <- [x-1..x+1]
  , y' <- [y-1..y+1]
  , z' <- [z-1..z+1]
  , not (x' == x && y' == y && z' == z)
  ]

willBecomeActive :: Grid -> Position -> Bool
willBecomeActive grid pos = not (isActive grid pos)
  && length (filter (isActive grid) (getAdjacentPositions pos)) == 3

willBecomeInactive :: Grid -> Position -> Bool
willBecomeInactive grid pos =
  let numOfActiveAdjacentCubes = length (filter (isActive grid) (getAdjacentPositions pos))
  in isActive grid pos
    && numOfActiveAdjacentCubes /= 2
    && numOfActiveAdjacentCubes /= 3

computeNextStateForCube :: Grid -> Position -> Cube -> Cube
computeNextStateForCube grid pos cube
  | willBecomeActive grid pos   = '#'
  | willBecomeInactive grid pos = '.'
  | otherwise = cube

computeNextState :: (Int,Int) -> (Int,Int) -> Grid -> Grid
computeNextState (widthFrom,widthTo) (heightFrom,heightTo) grid =
  let (depthFrom,depthTo) = (-6,6)  -- Initial plane +6 max in both directions
      allPos =
        [ (x,y,z)
        | x <- [widthFrom..widthTo]
        , y <- [heightFrom..heightTo]
        , z <- [depthFrom..depthTo]
        ]
  in foldr
    (\pos newGrid ->
      M.insert
        pos
        (computeNextStateForCube grid pos (if pos `M.member` grid then grid ! pos else '.'))
        newGrid)
    grid
    allPos

buildInitialGrid :: Plane -> ((Int,Int),(Int,Int),Grid)
buildInitialGrid plane =
  ((negativeHalfOfWidth-6, positiveHalfOfWidth+6), (negativeHalfOfHeight-6, positiveHalfOfHeight+6), grid)  -- Grid can grow +6 max in both directions
    where
      height = length plane
      width  = length (head plane)
      positiveHalfOfHeight = uncurry (+) ((height-1) `divMod` 2)
      negativeHalfOfHeight = -((height-1) `div` 2)
      positiveHalfOfWidth = uncurry (+) ((width-1) `divMod` 2)
      negativeHalfOfWidth = -((width-1) `div` 2)
      heightRange = [positiveHalfOfHeight, positiveHalfOfHeight-1 .. negativeHalfOfHeight]
      widthRange  = [negativeHalfOfWidth .. positiveHalfOfWidth]
      grid = M.fromList $ concat $ zipWith (\j row -> zipWith (\i cube -> ((i,j,0), cube)) widthRange row) heightRange plane

getSixthIteration :: ((Int,Int),(Int,Int),Grid) -> Grid
getSixthIteration (wRange,hRange,grid) = iterate (computeNextState wRange hRange) grid !! 6

countActiveCubes :: Grid -> Int
countActiveCubes = length . M.filter (=='#')

main :: IO ()
main = interact $ show . countActiveCubes . getSixthIteration . buildInitialGrid . lines
