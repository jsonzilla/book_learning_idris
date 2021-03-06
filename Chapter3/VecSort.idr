import Data.Vect

total allLength : Vect len String -> Vect len Nat
allLength [] = []
allLength (word :: words) = length word :: allLength words

total insert : Ord elem => (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

total insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in insert x xsSorted
