module PokerHand
where
import Data.List (sort, group)
import Data.Ord (comparing)

type Card = Int

data Category = HighCard | Pair
    deriving (Eq, Ord, Show)

data Hand = Hand [Card]
    deriving (Eq, Show)

instance Ord Hand
    where
    compare h g = case comparing category h g of 
                                  EQ -> comparing (reverse.sort.cards) h g
                                  r  -> r

category :: Hand -> Category
category h = case length (group (cards h)) of
                4 -> Pair
                _ -> HighCard

cards :: Hand -> [Card]
cards (Hand cs) = cs
