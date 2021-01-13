#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base ^>= 4.14.1.0
             , containers ^>= 0.6.2.1
             , split ^>= 0.2.3.4
-}

import Data.Bifunctor (second)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (foldl', isPrefixOf, sortOn)
import Data.List.Split (splitOn, splitWhen)
import Data.Set (Set, (\\))
import Text.ParserCombinators.ReadP (ReadP, char, munch1, readP_to_S, string)

import qualified Data.Set as S

type Range = (Int,Int)
type Ticket = [Int]
data Rule = Rule String Range Range deriving Show

applyRule :: Rule -> Int -> Bool
applyRule (Rule _ (min1,max1) (min2,max2)) n = (min1 <= n && n <= max1) || (min2 <= n && n <= max2)

applyRules :: [Rule] -> Int -> [Bool]
applyRules = traverse applyRule

removeInvalidTickets :: [Rule] -> [Ticket] -> [Ticket]
removeInvalidTickets rules = filter (all (or . applyRules rules))

determinePotentialFields :: [Rule] -> [Ticket] -> [Set String]
determinePotentialFields rules ([]:_)  = []
determinePotentialFields rules tickets =
  let col = map head tickets
      fieldsColCouldBe = foldl'
        (\acc rule@(Rule field _ _) ->
          if all (applyRule rule) col
          then S.insert field acc
          else acc)
        S.empty
        rules
  in fieldsColCouldBe : determinePotentialFields rules (map tail tickets)

uniquelyDetermineFields :: [(Int,Set String)] -> Set String -> [(Int,String)]
uniquelyDetermineFields [] _ = []
uniquelyDetermineFields (pf@(idx,potentialFields):pfs) seenFields =
  let unseenFields = potentialFields \\ seenFields
  in if S.size unseenFields == 1
    then (idx, head (S.toList unseenFields))
      : uniquelyDetermineFields pfs (S.insert (head (S.toList unseenFields)) seenFields)
    else uniquelyDetermineFields (pfs ++ [pf]) seenFields

uniquelyDetermineFieldsInOrder :: [Rule] -> [Ticket] -> [String]
uniquelyDetermineFieldsInOrder rules tickets =
  let potentialFields = determinePotentialFields rules tickets
      potentialFieldsWithIndices = sortOn (S.size . snd) $ zip [0..] potentialFields
  in map snd $ sortOn fst $ uniquelyDetermineFields potentialFieldsWithIndices S.empty

collectDepartureFields :: ([Rule],Ticket,[Ticket]) -> [Int]
collectDepartureFields (rules,myTicket,otherTickets) =
  let validTickets = removeInvalidTickets rules otherTickets
      fieldsInOrder = uniquelyDetermineFieldsInOrder rules validTickets
  in map snd . filter (isPrefixOf "departure" . fst) $ zip fieldsInOrder myTicket

parseRule :: ReadP Rule
parseRule = do
  field <- munch1 (\c -> isAlpha c || isSpace c)
  string ": "
  min1 <- read <$> munch1 isDigit
  char '-'
  max1 <- read <$> munch1 isDigit
  string " or "
  min2 <- read <$> munch1 isDigit
  char '-'
  max2 <- read <$> munch1 isDigit
  return $ Rule field (min1, max1) (min2, max2)

parseRules :: [String] -> [Rule]
parseRules = map (fst . head . readP_to_S parseRule)

parseInput :: [[String]] -> ([Rule],Ticket,[Ticket])
parseInput (rules:[_,myTicket]:[_:otherTickets]) =
  (parseRules rules, map read (splitOn "," myTicket), map (map read . splitOn ",") otherTickets)

main :: IO ()
main = interact $ show . product . collectDepartureFields . parseInput . splitWhen null . lines
