module Counts

count : String -> (Nat, Nat)
count str = (length (words str), length str)

topTen : Ord a => List a -> List a
topTen xs = take 10 (reverse (sort xs))

overLength : Nat -> List String -> Nat
overLength c xs = length (filter(\x => x >= c) (map length xs))
