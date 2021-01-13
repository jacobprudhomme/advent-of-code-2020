#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base, containers, split
-}

import Control.Arrow ((&&&))
import Data.Bits (clearBit, setBit)
import Data.IntMap.Strict (IntMap)
import Data.List (foldl', isPrefixOf)
import Data.List.Split (splitWhen)

import qualified Data.IntMap.Strict as M

type Address = Int
type Data = Int
type Mask = Int -> Int
type Memory = IntMap Int
type AddressDataPair = (Address,Data)

parseMask :: String -> Mask
parseMask = fst . foldr determineMaskAtBit (id, 0) . drop 2 . dropWhile (/='=')
  where
    determineMaskAtBit 'X' (mask,idx) = (mask, idx+1)
    determineMaskAtBit '0' (mask,idx) = (flip clearBit idx . mask, idx+1)
    determineMaskAtBit '1' (mask,idx) = (flip setBit idx . mask, idx+1)

parseAddress :: String -> Address
parseAddress = read . tail . takeWhile (/=']') . dropWhile (/='[')

parseData :: String -> Data
parseData = read . drop 2 . dropWhile (/='=')

parseProgramLine :: String -> AddressDataPair
parseProgramLine = parseAddress &&& parseData

parseMasks :: [String] -> [Mask]
parseMasks = map parseMask . filter ("mask" `isPrefixOf`)

parsePrograms :: [String] -> [[AddressDataPair]]
parsePrograms = map (map parseProgramLine) . tail . splitWhen ("mask" `isPrefixOf`)

splitIntoMasksAndPrograms :: [String] -> [(Mask,[AddressDataPair])]
splitIntoMasksAndPrograms lines = zip (parseMasks lines) (parsePrograms lines)

runProgram :: Memory -> (Mask,[AddressDataPair]) -> Memory
runProgram mem (mask,program) = foldl' insertData mem program
  where insertData mem (addr,val) = M.insert addr (mask val) mem

runPrograms :: [(Mask,[AddressDataPair])] -> Memory
runPrograms = foldl' runProgram M.empty

main :: IO ()
main = interact $ show . sum . runPrograms . splitIntoMasksAndPrograms . lines
