module PokerHand
where
import Data.List (sort, group)
import Data.Ord (comparing)


type Card = Int

data Category = HighCard | Pair | TwoPair
    deriving (Eq, Ord, Show)

data Hand = Hand [Card]
    deriving (Eq, Show)

instance Ord Hand
    where
    compare h g = case comparing category h g of 
                                  EQ -> comparing (reverse.sort.cards) h g
                                  r  -> r

rsort :: Ord a => [a] -> [a]
rsort = reverse . sort

category :: Hand -> Category
category h = case rsort(map length(group (cards h))) of
                [2,2,1] -> TwoPair
                [2,1,1,1] -> Pair
                _ -> HighCard

cards :: Hand -> [Card]
cards (Hand cs) = cs
