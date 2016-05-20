import Test.QuickCheck
import PokerHand
import Data.List (sort)

rsort = reverse . sort

instance Arbitrary Hand
    where
    arbitrary = vectorOf 5 arbitrary >>= (return.Hand)

highCardRelativeOrder h g = (category h == HighCard
                          && category g == HighCard)
                     ==> (h > g) ==>
    rsort (cards h) > rsort (cards g) 

main = quickCheck highCardRelativeOrder


