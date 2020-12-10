#!/usr/bin/env runhaskell

import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!))

import qualified Data.Map.Strict as M

parseCountAndReconstructColour :: [String] -> (String, Int)
parseCountAndReconstructColour (count:rest) = (unwords rest, read count)

splitContents :: String -> [(String, Int)]
splitContents "no other bags." = []
splitContents contents =
  let withoutPeriod = init contents
  in map (parseCountAndReconstructColour . init . words) $ splitOn ", " withoutPeriod

getBagAndContents :: String -> (String,[(String, Int)])
getBagAndContents rule =
  let [bag,contents] = splitOn " bags contain " rule
  in (bag, splitContents contents)

buildChildBags :: [String] -> Map String [(String, Int)]
buildChildBags = M.fromList . map getBagAndContents

countBagsContainedByShinyGold :: Map String [(String, Int)] -> Int
countBagsContainedByShinyGold bagRules = go "shiny gold" - 1  -- Do not count the shiny gold bag itself
  where
    go colour =
      let mustContain = bagRules ! colour
      in if null mustContain then 1 else foldr (\(colour, count) acc -> count * go colour + acc) 1 mustContain

main :: IO ()
main = interact $ show . countBagsContainedByShinyGold . buildChildBags . lines
