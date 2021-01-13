#!/usr/bin/env -S cabal run -O2 -v0
{- cabal:
build-depends: base ^>= 4.14.1.0
             , containers ^>= 0.6.2.1
             , split ^>= 0.2.3.4
-}

import Data.List.Split (splitWhen)

import qualified Data.Set as S

type Card = Int
type Deck = [Card]
data Winner = Player1 | Player2

playSubgame :: (Deck,Deck) -> Winner
playSubgame (card1:deck1,card2:deck2) = fst (play (deck1', deck2'))
  where
    deck1' = take card1 deck1
    deck2' = take card2 deck2

play :: (Deck,Deck) -> (Winner,Deck)
play = flip go S.empty
  where
    winRound (card1:deck1,card2:deck2) player = case player of
      Player1 -> (deck1 ++ [card1, card2], deck2)
      Player2 -> (deck1, deck2 ++ [card2, card1])
    go (deck1,[]) _ = (Player1, deck1)
    go ([],deck2) _ = (Player2, deck2)
    go decks@(card1:deck1,card2:deck2) seenDecks
      | decks `S.member` seenDecks = (Player1, card1:deck1)
      | card1 <= length deck1 && card2 <= length deck2 =
        go (winRound decks (playSubgame decks)) (S.insert decks seenDecks)
      | card1 >= card2 = go (winRound decks Player1) (S.insert decks seenDecks)
      | otherwise      = go (winRound decks Player2) (S.insert decks seenDecks)

calculateScore :: (Winner,Deck) -> Int
calculateScore (_,winningDeck) = sum $ zipWith (*) [1..] $ reverse winningDeck

parseDecks :: [[String]] -> (Deck,Deck)
parseDecks [_:deck1,_:deck2] = (map read deck1, map read deck2)

main :: IO ()
main = interact $ show . calculateScore . play . parseDecks . splitWhen null . lines
