import Test.QuickCheck
import PokerHand
import Data.List (sort)

rsort = reverse . sort

instance Arbitrary Hand
    where
    arbitrary = frequency
                [(50, vectorOf 5 arbitrary >>= (return.Hand))
                ,(50, vectorOf 4 arbitrary >>= \cs -> return (Hand (head cs:cs)))]

highCardRelativeOrder h g = (category h == HighCard
                          && category g == HighCard)
                     ==> (h > g) ==>
    rsort (cards h) > rsort (cards g) 

pairToHighCardOrder h g = (category h == Pair
                        && category g == HighCard)
                     ==> h > g
main = do 
    quickCheck highCardRelativeOrder
    quickCheck pairToHighCardOrder


