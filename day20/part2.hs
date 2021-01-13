#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base ^>= 4.14.1.0
             , array ^>= 0.5.4.0
             , containers ^>= 0.6.2.1
             , ilist ^>= 0.4.0.1
             , split ^>= 0.2.3.4
             , transformers ^>= 0.5.6.2
-}

{-# LANGUAGE TupleSections #-}

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Control.Monad.Trans.State.Strict (State, evalState, get, modify)
import Data.Array.Unboxed
import Data.Bifunctor (bimap, first, second)
import Data.Char (isDigit)
import Data.List (find)
import Data.List.Index (ifoldl')
import Data.List.Split (splitWhen)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromJust, isJust)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type ID = Int
type Tile = [[Char]]
type Side = [Char]
type Position = (Int,Int)
type Grid = Map Position (ID,Tile)
type UnprocessedNeighbours = State [(ID,Tile)]

data SideOrientation
  = TopN
  | TopR
  | RightN
  | RightR
  | BottomN
  | BottomR
  | LeftN
  | LeftR
  deriving (Enum, Eq, Show)

getTopSide :: Tile -> Side
getTopSide = head

getRightSide :: Tile -> Side
getRightSide = map last

getBottomSide :: Tile -> Side
getBottomSide = last

getLeftSide :: Tile -> Side
getLeftSide = map head

reverseOrientation :: SideOrientation -> SideOrientation
reverseOrientation so
  | so `elem` [TopN, RightN, BottomN, LeftN] = succ so
  | otherwise = so

possibleSides :: [Tile -> (SideOrientation,Side)]
possibleSides = (.) <$> [id, bimap reverseOrientation reverse] <*>
  [ (TopN,) . getTopSide
  , (RightN,) . getRightSide
  , (BottomN,) . getBottomSide
  , (LeftN,) . getLeftSide
  ]

getPossibleSides :: Tile -> [(SideOrientation,Side)]
getPossibleSides tile = map ($ tile) possibleSides

flipHorizontally :: Tile -> Tile
flipHorizontally = reverse

flipVertically :: Tile -> Tile
flipVertically = map reverse

-- Rotation 90deg to the right
rotate :: Tile -> Tile
rotate ([]:_) = []
rotate tile   = reverse (map head tile) : rotate (map tail tile)

getTopNeighbour :: (ID,Tile) -> [(ID,Tile)] -> Maybe (ID,Tile)
getTopNeighbour _ [] = Nothing
getTopNeighbour t@(_,tile) ((otherId,otherTile):otherTiles) =
  case find ((==getTopSide tile) . snd) (getPossibleSides otherTile) of
    Nothing -> getTopNeighbour t otherTiles
    Just (orientation,_) -> Just $ (otherId,) $ case orientation of
      TopN -> flipHorizontally otherTile
      TopR -> rotate . rotate $ otherTile
      RightN -> flipVertically . rotate $ otherTile
      RightR -> rotate otherTile
      BottomN -> otherTile
      BottomR -> flipVertically otherTile
      LeftN -> rotate . rotate . rotate $ otherTile
      LeftR -> flipHorizontally . rotate $ otherTile

getRightNeighbour :: (ID,Tile) -> [(ID,Tile)] -> Maybe (ID,Tile)
getRightNeighbour _ [] = Nothing
getRightNeighbour t@(_,tile) ((otherId,otherTile):otherTiles) =
  case find ((==getRightSide tile) . snd) (getPossibleSides otherTile) of
    Nothing -> getRightNeighbour t otherTiles
    Just (orientation,_) -> Just $ (otherId,) $ case orientation of
      TopN -> flipVertically . rotate $ otherTile
      TopR -> rotate . rotate . rotate $ otherTile
      RightN -> flipVertically otherTile
      RightR -> rotate . rotate $ otherTile
      BottomN -> rotate otherTile
      BottomR -> flipHorizontally . rotate $ otherTile
      LeftN -> otherTile
      LeftR -> flipHorizontally otherTile

getBottomNeighbour :: (ID,Tile) -> [(ID,Tile)] -> Maybe (ID,Tile)
getBottomNeighbour _ [] = Nothing
getBottomNeighbour t@(_,tile) ((otherId,otherTile):otherTiles) =
  case find ((==getBottomSide tile) . snd) (getPossibleSides otherTile) of
    Nothing -> getBottomNeighbour t otherTiles
    Just (orientation,_) -> Just $ (otherId,) $ case orientation of
      TopN -> otherTile
      TopR -> flipVertically otherTile
      RightN -> rotate . rotate . rotate $ otherTile
      RightR -> flipHorizontally . rotate $ otherTile
      BottomN -> flipHorizontally otherTile
      BottomR -> rotate . rotate $ otherTile
      LeftN -> flipVertically . rotate $ otherTile
      LeftR -> rotate otherTile

getLeftNeighbour :: (ID,Tile) -> [(ID,Tile)] -> Maybe (ID,Tile)
getLeftNeighbour _ [] = Nothing
getLeftNeighbour t@(_,tile) ((otherId,otherTile):otherTiles) =
  case find ((==getLeftSide tile) . snd) (getPossibleSides otherTile) of
    Nothing -> getLeftNeighbour t otherTiles
    Just (orientation,_) -> Just $ (otherId,) $ case orientation of
      TopN -> rotate otherTile
      TopR -> flipHorizontally . rotate $ otherTile
      RightN -> otherTile
      RightR -> flipHorizontally otherTile
      BottomN -> flipVertically . rotate $ otherTile
      BottomR -> rotate . rotate . rotate $ otherTile
      LeftN -> flipVertically otherTile
      LeftR -> rotate . rotate $ otherTile

addNeighbours :: [(ID,Tile)] -> Grid
addNeighbours (tile:otherTiles) = evalState (go tile (0,0)) otherTiles
  where
    go :: (ID,Tile) -> Position -> UnprocessedNeighbours Grid
    go tile pos = do
      unprocessedNeighbours <- get
      let topNeighbour = getTopNeighbour tile unprocessedNeighbours
          rightNeighbour = getRightNeighbour tile unprocessedNeighbours
          bottomNeighbour = getBottomNeighbour tile unprocessedNeighbours
          leftNeighbour = getLeftNeighbour tile unprocessedNeighbours
          neighbours = [topNeighbour, rightNeighbour, bottomNeighbour, leftNeighbour]
          neighboursWithPosMod = map (second fromJust) $ filter (isJust . snd) $
            zip [second (+1), first (+1), second (subtract 1), first (subtract 1)] neighbours
      modify $ filter (\(_,otherTile) ->
          S.fromList (map snd (getPossibleSides otherTile)) `notElem`
            map (S.fromList . map snd . getPossibleSides . snd) (catMaybes neighbours))
      recursiveNeighbours <- mapM (\(posMod,neighbour) -> go neighbour (posMod pos)) neighboursWithPosMod
      return $ M.unions $ M.singleton pos tile : recursiveNeighbours

relativeSeaMonsterPositions :: [Position]
relativeSeaMonsterPositions =
  [ (0,0),  (1,1),  (4,1),   (5,0),  (6,0)
  , (7,1),  (10,1), (11,0),  (12,0), (13,1)
  , (16,1), (17,0), (18,-1), (18,0), (19,0)
  ]

getRelativeSeaMonsterPositions :: Position -> [Position]
getRelativeSeaMonsterPositions (x,y) = map (\(x',y') -> (x+x', y+y')) relativeSeaMonsterPositions

-- Rotation 90deg to the right
rotateArray :: UArray Position Char -> UArray Position Char
rotateArray xxs =
  let bnds@(_,(maxX,maxY)) = bounds xxs
  in ixmap bnds (\(x,y) -> (y, maxX-x)) xxs

flipArrayHorizontally :: UArray Position Char -> UArray Position Char
flipArrayHorizontally xxs =
  let bnds@(_,(maxX,maxY)) = bounds xxs
  in ixmap bnds (\(x,y) -> (maxX-x, y)) xxs

flipArrayVertically :: UArray Position Char -> UArray Position Char
flipArrayVertically xxs =
  let bnds@(_,(maxX,maxY)) = bounds xxs
  in ixmap bnds (\(x,y) -> (x, maxY-y)) xxs

arrayTransforms :: [UArray Position Char -> UArray Position Char]
arrayTransforms =
  [ id
  , rotateArray
  , rotateArray . rotateArray
  , rotateArray . rotateArray . rotateArray
  , flipArrayHorizontally
  , flipArrayVertically
  , flipArrayHorizontally . rotateArray
  , flipArrayVertically . rotateArray
  ]

countSeaMonsters :: UArray Position Char -> Int
countSeaMonsters t = sum [ 1 | x <- [xFrom..xTo], y <- [yFrom..yTo], tile <- tiles, isSeaMonsterAt tile (x,y) ]
  where
    tiles = map ($ t) arrayTransforms
    isSeaMonsterAt :: UArray Position Char -> Position -> Bool
    isSeaMonsterAt tile = all ((=='#') . (tile !)) . getRelativeSeaMonsterPositions
    -- These next values are to search within bounds of tile
    (_,(maxX,maxY)) = bounds t
    xFrom = 0
    xTo = maxX - 19  -- Sea monster is 19 pixels wide
    yFrom = 1  -- Sea monster reaches 1 pixel higher than current position
    yTo = maxY - 1  -- Sea monster reaches 1 pixel lower than current position

countRoughWaterPixels :: UArray Position Char -> Int
countRoughWaterPixels t =
  let ((xFrom,yFrom),(xTo,yTo)) = bounds t
      numOfSeaMonsters = countSeaMonsters t
      roughWaterPixelsInSeaMonster = length relativeSeaMonsterPositions
      roughWaterPixels = sum [ 1 | x <- [xFrom..xTo], y <- [yFrom..yTo], t ! (x,y) == '#' ]
  in roughWaterPixels - (numOfSeaMonsters * roughWaterPixelsInSeaMonster)

splitTileAndId :: [String] -> (ID,Tile)
splitTileAndId = read . init . dropWhile (not . isDigit) . head &&& tail

removeBorders :: Tile -> Tile
removeBorders = init . tail . map (init . tail)

mergeTiles :: Grid -> Tile
mergeTiles grid =
  foldr (\y squareOfTiles ->
      foldr (\x rowOfTiles ->
          zipWith (++) (removeBorders (snd (grid M.! (x,y)))) rowOfTiles)
        (repeat [])
        [minX..maxX]
      ++ squareOfTiles)
    []
    [maxY,maxY-1..minY]
    where
      coords = M.keys grid
      xCoords = map fst coords
      yCoords = map snd coords
      minX = minimum xCoords
      maxX = maximum xCoords
      minY = minimum yCoords
      maxY = maximum yCoords

tileToArray :: Tile -> UArray Position Char
tileToArray tile =
  let maxX = length (head tile) - 1
      maxY = length tile - 1
  in array ((0,0), (maxX,maxY)) $ concat $ ifoldl' (\tileAcc y row -> ifoldl' (\rowAcc x pixel -> ((x,y), pixel) : rowAcc) [] row : tileAcc) [] tile

main :: IO ()
main = interact $ show . countRoughWaterPixels . tileToArray . mergeTiles . addNeighbours . map splitTileAndId . splitWhen null . lines
