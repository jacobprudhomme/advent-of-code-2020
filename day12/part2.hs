import Prelude hiding (Left, Right)

import Data.Bifunctor (first, second)
import Data.List (foldl')

data Direction = Left | Right deriving Eq
data Heading = North | East | South | West deriving (Enum, Show)

type Instruction = (Char,Int)
type Position = (Int,Int)
type WaypointShipPosition = (Position,Position)

moveInHeading :: Heading -> Int -> WaypointShipPosition -> WaypointShipPosition
moveInHeading North d = first (second (+d))
moveInHeading East  d = first (first  (+d))
moveInHeading South d = first (second (subtract d))
moveInHeading West  d = first (first  (subtract d))

moveForward :: Int -> WaypointShipPosition -> WaypointShipPosition
moveForward 0 pos = pos
moveForward n (wayptPos@(dew,dns),(ew,ns)) = moveForward (n-1) (wayptPos, (ew+dew, ns+dns))

rotateLeft :: WaypointShipPosition -> WaypointShipPosition
rotateLeft ((dew,dns),shipPos) = ((-dns,dew),shipPos)

rotateRight :: WaypointShipPosition -> WaypointShipPosition
rotateRight ((dew,dns),shipPos) = ((dns,-dew),shipPos)

turn :: Direction -> Int -> WaypointShipPosition -> WaypointShipPosition
turn dir d pos
  | dir == Left = iterate rotateLeft pos !! quadrantsToRotate
  | otherwise   = iterate rotateRight pos !! quadrantsToRotate
  where quadrantsToRotate = d `div` 90

parseInstruction :: String -> Instruction
parseInstruction (instr:arg) = (instr, read arg)

executeInstruction :: Char -> Int -> WaypointShipPosition -> WaypointShipPosition
executeInstruction 'N' = moveInHeading North
executeInstruction 'E' = moveInHeading East
executeInstruction 'S' = moveInHeading South
executeInstruction 'W' = moveInHeading West
executeInstruction 'L' = turn Left
executeInstruction 'R' = turn Right
executeInstruction 'F' = moveForward

sumDistances :: WaypointShipPosition -> Int
sumDistances (_,(ew,ns)) = abs ew + abs ns

initialPos :: WaypointShipPosition
initialPos = ((10, 1), (0, 0))

main :: IO ()
main = interact $ show . sumDistances . foldl' (flip (uncurry executeInstruction . parseInstruction)) initialPos . lines
