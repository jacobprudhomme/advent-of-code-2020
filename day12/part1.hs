#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base
-}

import Prelude hiding (Left, Right)

import Data.Bifunctor (first, second)
import Data.List (foldl')

data Direction = Left | Right deriving Eq
data Heading = North | East | South | West deriving (Enum, Show)

type Instruction = (Char,Int)
type Position = (Heading,Int,Int)

moveInHeading :: Heading -> Int -> Position -> Position
moveInHeading North d = second (+d)
moveInHeading East  d = first  (+d)
moveInHeading South d = second (subtract d)
moveInHeading West  d = first  (subtract d)

moveForward :: Int -> Position -> Position
moveForward d pos@(hdg,_,_) = moveInHeading hdg d pos

pred' :: Heading -> Heading
pred' North = West
pred' x     = pred x

succ' :: Heading -> Heading
succ' West = North
succ' x    = succ x

turn :: Direction -> Int -> Position -> Position
turn dir d (hdg,ew,ns)
  | dir == Left = (iterate pred' hdg !! quadrantsToRotate, ew, ns)
  | otherwise   = (iterate succ' hdg !! quadrantsToRotate, ew, ns)
  where quadrantsToRotate = d `div` 90

parseInstruction :: String -> Instruction
parseInstruction (instr:arg) = (instr, read arg)

executeInstruction :: Char -> Int -> Position -> Position
executeInstruction 'N' = moveInHeading North
executeInstruction 'E' = moveInHeading East
executeInstruction 'S' = moveInHeading South
executeInstruction 'W' = moveInHeading West
executeInstruction 'L' = turn Left
executeInstruction 'R' = turn Right
executeInstruction 'F' = moveForward

sumDistances :: Position -> Int
sumDistances (_,ew,ns) = abs ew + abs ns

initialPos :: Position
initialPos = (East, 0, 0)

main :: IO ()
main = interact $ show . sumDistances . foldl' (flip (uncurry executeInstruction . parseInstruction)) initialPos . lines
