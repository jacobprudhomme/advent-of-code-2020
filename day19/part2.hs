import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.IntMap.Strict (IntMap, (!))
import Text.ParserCombinators.ReadP

import qualified Data.IntMap.Strict as M

data Rule
  = Match Char
  | RuleRef Int
  | All [Rule]
  | Any [Rule]
  deriving Show

parseRule :: ReadP (Int,Rule)
parseRule = pKVPair
  where
    pMatch = Match <$> (char '"' *> get <* char '"')
    pRuleRef = RuleRef . read <$> munch1 isDigit
    pAll = All <$> ((:) <$> pRuleRef <*> (char ' ' *> sepBy1 pRuleRef (char ' ')))
    pAny = Any <$> ((:) <$> (pRuleRef +++ pAll) <*> (string " | " *> sepBy1 (pRuleRef +++ pAll) (string " | ")))
    pKVPair = (,) <$> (read <$> munch1 isDigit) <*> (string ": " *> choice [pMatch, pRuleRef, pAll, pAny])

parseRules :: [String] -> IntMap Rule
parseRules = M.insert 11 newRule2 . M.insert 8 newRule1 . M.fromList . map (fst . last . readP_to_S parseRule)
  where
    newRule1 = Any [RuleRef 42, All [RuleRef 42, RuleRef 8]]
    newRule2 = Any [All [RuleRef 42, RuleRef 31], All [RuleRef 42, RuleRef 11, RuleRef 31]]

buildParserFromRules :: IntMap Rule -> ReadP Char
buildParserFromRules rules = go (rules ! 0)
  where
    go (Match c) = char c
    go (RuleRef rr) = go (rules ! rr)
    go (All rs) = head <$> traverse go rs
    go (Any rs) = choice (map go rs)

matchRules :: ReadP Char -> [String] -> [Bool]
matchRules rulesParser = map (matches . readP_to_S rulesParser)
  where
    matches parseResult = case parseResult of
      [] -> False
      xs -> (=="") $ snd $ last xs

countMatches :: [Bool] -> Int
countMatches = length . filter id

main :: IO ()
main = interact $ show . countMatches . uncurry matchRules . bimap (buildParserFromRules . parseRules) tail . break null . lines
