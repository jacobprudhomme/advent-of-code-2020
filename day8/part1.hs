import Control.Monad.Trans.State.Strict (State, evalState, gets, modify')
import Data.Bifunctor (first, second)
import Data.Set (Set)

import qualified Data.Set as S

type Accumulator = Int
type Instruction = (String,Int)
type InstructionPointer = Int
type SeenInstructions = Set Int
type Program = ([Instruction], SeenInstructions, InstructionPointer, Accumulator)
type ProgramState = State Program

getCurrentInstruction :: ProgramState Instruction
getCurrentInstruction = do
  ip <- gets (\(_,_,ip,_) -> ip)
  gets (\(instrs,_,_,_) -> instrs !! (ip-1))

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

run :: ProgramState Accumulator
run = do
  (cmd,arg) <- getCurrentInstruction
  ip <- getInstructionPointer
  hasBeenSeen <- hasInstructionBeenSeen ip
  if hasBeenSeen
  then getAccumulator
  else case cmd of
    "acc" -> updateAccumulator arg >> addToSeenInstructions ip >> goToNextInstruction >> run
    "jmp" -> jumpToInstruction arg >> addToSeenInstructions ip >> run
    "nop" -> addToSeenInstructions ip >> goToNextInstruction >> run

runProgram :: Program -> Accumulator
runProgram = evalState run

initializeState :: [Instruction] -> Program
initializeState instrs = (instrs, S.empty, 1, 0)

parseInstruction :: String -> Instruction
parseInstruction = (\[cmd,arg] -> (cmd, parseInt arg)) . words
  where
    parseInt intStr =
      let sign = head intStr
          int = read $ tail intStr
      in if sign == '+' then int else (-int)

main :: IO ()
main = interact $ show . runProgram . initializeState . map parseInstruction . lines
