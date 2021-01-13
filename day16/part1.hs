#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base, split
-}

import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (foldl')
import Data.List.Split (splitOn, splitWhen)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, readP_to_S, string)

type Range = (Int,Int)
type Ticket = [Int]
data Rule = Rule Range Range deriving Show

applyRule :: Rule -> Int -> Bool
applyRule (Rule (min1,max1) (min2,max2)) n = (min1 <= n && n <= max1) || (min2 <= n && n <= max2)

applyRules :: [Rule] -> Int -> [Bool]
applyRules = traverse applyRule

sumOfNumbersThatDontMeetRulesForTicket :: [Rule] -> Ticket -> Int
sumOfNumbersThatDontMeetRulesForTicket rules =
  foldl'
    (\acc num ->
      if not (or (applyRules rules num))
      then num + acc
      else acc)
    0

sumOfNumbersThatDontMeetRules :: [Rule] -> [Ticket] -> Int
sumOfNumbersThatDontMeetRules rules =
  foldl'
    (\acc ticket ->
      sumOfNumbersThatDontMeetRulesForTicket rules ticket + acc)
    0

parseRule :: ReadP Rule
parseRule = do
  munch1 (\c -> isAlpha c || isSpace c)
  string ": "
  min1 <- read <$> munch1 isDigit
  char '-'
  max1 <- read <$> munch1 isDigit
  string " or "
  min2 <- read <$> munch1 isDigit
  char '-'
  max2 <- read <$> munch1 isDigit
  return $ Rule (min1, max1) (min2, max2)

parseRules :: [String] -> [Rule]
parseRules = map (fst . head . readP_to_S parseRule)

parseInput :: [[String]] -> ([Rule],[Ticket])
parseInput (rules:_:[_:otherTickets]) =
  (parseRules rules, map (map read . splitOn ",") otherTickets)

main :: IO ()
main = interact $ show . uncurry sumOfNumbersThatDontMeetRules . parseInput . splitWhen null . lines
