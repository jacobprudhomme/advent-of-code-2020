#!/usr/bin/env cabal
{- cabal:
build-depends: base, split
-}

import Data.List.Split (splitWhen)

type Deck = [Int]

play :: (Deck,Deck) -> Deck
play ([],deck2) = deck2
play (deck1,[]) = deck1
play (card1:deck1,card2:deck2)
  | card1 >= card2 = play (deck1 ++ [card1, card2], deck2)
  | otherwise      = play (deck1, deck2 ++ [card2, card1])

calculateScore :: Deck -> Int
calculateScore = sum . zipWith (*) [1..] . reverse

parseDecks :: [[String]] -> (Deck,Deck)
parseDecks [_:deck1,_:deck2] = (map read deck1, map read deck2)

main :: IO ()
main = interact $ show . calculateScore . play . parseDecks . splitWhen null . lines
