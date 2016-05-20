module PokerHand
where
import Data.List (sort)
import Data.Ord (comparing)

type Card = Int

data Hand = Hand [Card]
    deriving (Eq, Show)

instance Ord Hand
    where
    compare = comparing (reverse.sort.cards)
data Category = HighCard
    deriving (Eq, Ord, Show)

category :: Hand -> Category
category _ = HighCard

cards :: Hand -> [Card]
cards (Hand cs) = cs
