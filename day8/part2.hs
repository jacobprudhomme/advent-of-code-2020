#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base ^>= 4.14.1.0
             , containers ^>= 0.6.2.1
             , transformers ^>= 0.5.6.2
-}

import Control.Monad.Trans.State.Strict (State, evalState, gets, modify')
import Data.Bifunctor (first, second)
import Data.List (nub)
import Data.Set (Set)

import qualified Data.Set as S

type Accumulator = Int
type Instruction = (String,Int)
type InstructionPointer = Int
type SeenInstructions = Set Int
type Program = ([Instruction], SeenInstructions, InstructionPointer, Accumulator)
type ProgramState = State Program

getInstructions :: ProgramState [Instruction]
getInstructions = gets (\(instrs,_,_,_) -> instrs)

getInstructionPointer :: ProgramState InstructionPointer
getInstructionPointer = gets (\(_,_,ip,_) -> ip)

getAccumulator :: ProgramState Accumulator
getAccumulator = gets (\(_,_,_,acc) -> acc)

goToNextInstruction :: ProgramState ()
goToNextInstruction = modify' (first (+1))

jumpToInstruction :: Int -> ProgramState ()
jumpToInstruction offset = modify' (first (+offset))

updateAccumulator :: Int -> ProgramState ()
updateAccumulator amount = modify' (second (+amount))

addToSeenInstructions :: InstructionPointer -> ProgramState ()
addToSeenInstructions instrLine = modify' (\(is,sis,ip,acc) -> (is, S.insert instrLine sis, ip, acc))

hasInstructionBeenSeen :: InstructionPointer -> ProgramState Bool
hasInstructionBeenSeen instrLine = do
  seenInstrs <- gets (\(_,seenInstrs,_,_) -> seenInstrs)
  return $ S.member instrLine seenInstrs

run :: ProgramState (Accumulator,Bool)
run = do
  ip <- getInstructionPointer
  instrs <- getInstructions
  hasBeenSeen <- hasInstructionBeenSeen ip
  if hasBeenSeen || ip >= length instrs
    then getAccumulator >>= (\acc -> pure (acc, hasBeenSeen))
    else do
      let (cmd,arg) = instrs !! ip
      case cmd of
        "acc" -> updateAccumulator arg >> addToSeenInstructions ip >> goToNextInstruction >> run
        "jmp" -> jumpToInstruction arg >> addToSeenInstructions ip >> run
        "nop" -> addToSeenInstructions ip >> goToNextInstruction >> run

noInfiniteLoops :: (Accumulator,Bool) -> Bool
noInfiniteLoops (_,hasBeenSeen) = not hasBeenSeen

runProgram :: Program -> (Accumulator,Bool)
runProgram = evalState run

initializeState :: [Instruction] -> Program
initializeState instrs = (instrs, S.empty, 1, 0)

switchCommand :: Int -> [Instruction] -> [Instruction]
switchCommand 0 instrs = instrs
switchCommand idx instrs =
  [ if i == idx && cmd == "jmp" then ("nop", arg) else if i == idx && cmd == "nop" then ("jmp", arg) else instr
  | (i,instr@(cmd,arg)) <- zip [1..] instrs ]

switchCommands :: [Instruction] -> [[Instruction]]
switchCommands instrs =
  let len = length instrs
  in nub [ switchCommand i instrs | i <- [0..len-1] ]

parseInstruction :: String -> Instruction
parseInstruction = (\[cmd,arg] -> (cmd, parseInt arg)) . words
  where
    parseInt intStr =
      let sign = head intStr
          int = read $ tail intStr
      in if sign == '+' then int else (-int)

main :: IO ()
main = interact $ show . fst . head . filter noInfiniteLoops . map (runProgram . initializeState) . switchCommands . map parseInstruction . lines
