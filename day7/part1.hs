import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!))

import qualified Data.Map.Strict as M

splitContents :: String -> [String]
splitContents "no other bags." = []
splitContents contents =
  let withoutPeriod = init contents
  in map (unwords . init . tail . words) $ splitOn ", " withoutPeriod

getBagAndContents :: String -> (String,[String])
getBagAndContents rule =
  let [bag,contents] = splitOn " bags contain " rule
  in (bag, splitContents contents)

buildChildBags :: [String] -> Map String [String]
buildChildBags = M.fromList . map getBagAndContents

countBagsContainingShinyGold :: Map String [String] -> Int
countBagsContainingShinyGold bagRules = M.foldrWithKey (\colour _ acc -> go colour + acc) 0 bagRules
  where
    go colour =
      let canContain = bagRules ! colour
      in if "shiny gold" `elem` canContain then 1 else foldr (max . go) 0 canContain

main :: IO ()
main = interact $ show . countBagsContainingShinyGold . buildChildBags . lines
