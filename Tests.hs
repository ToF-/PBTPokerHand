import Test.QuickCheck
import PokerHand
import Data.List (sort,group)
import Data.Ord (comparing)


instance Arbitrary Hand
    where
    arbitrary = frequency
                [(30, vectorOf 5 arbitrary >>= (return.Hand))
                ,(30, vectorOf 4 arbitrary >>= \cs -> return (Hand (head cs:cs)))
                ,(80, vectorOf 3 arbitrary >>= \[a,b,c] -> return (Hand [a,a,b,b,c]))]

highCardRelativeOrder :: Hand -> Hand -> Property
highCardRelativeOrder h g = (category h == HighCard
                          && category g == HighCard
                          && (h > g)) ==> (rsort (cards h)) > (rsort (cards g)) 

pairToHighCardOrder :: Hand -> Hand -> Property  
pairToHighCardOrder h g = (category h == Pair
                        && category g == HighCard)
                     ==> h > g

categoryOrder :: Hand -> Hand -> Property
categoryOrder h g = (category h > category g) ==> h > g

twoPairRelativeOrder :: Hand -> Hand -> Property
twoPairRelativeOrder h g = (category h == TwoPair
                        && category g == TwoPair
                        && h > g )
                    ==> (comparing (rsort.group.sort.cards) h g) == GT

main = do 
    quickCheck highCardRelativeOrder
    quickCheck pairToHighCardOrder
    quickCheck categoryOrder
    quickCheck twoPairRelativeOrder

