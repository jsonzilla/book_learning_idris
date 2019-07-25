module Main

import Data.Fin
import Data.Vect

main : IO ()
main = putStrLn "Hello world"

x : Int
x = 42

foo : String
foo = "Sausage machine"

bar : Char
bar = 'Z'

quux : Bool
quux = False

-- data Nat2 = S | S Nat2
--data List a = Nil | (::) a (List a)

-- infix 10 ::
plus2 : Nat -> Nat -> Nat
plus2 Z y = y
plus2 (S k) y = S (plus2 k y)

mult2 : Nat -> Nat -> Nat
mult2 Z y = Z
mult2 (S k) y = plus2 y (mult2 k y)

-- reverse : List a -> List a
-- reverse xs = revAcc [] xs where
--   revAcc : List a -> List a -> List a
--   revAcc acc [] = acc
--   revAcc acc (x :: xs) revAcc (x :: acc) xs

even : Nat -> Bool
even Z = True
even (S k) = odd k where
  odd Z = False
  odd (S k) = even k

test : List Nat
test = [c (S 1), c Z, d (S Z)]
  where c x = 42 + x
        d y = c (y + 1 + z y)
              where z w = y + w

-- index2 : Fin n -> Vect n a -> a
-- index2 FZ (x :: xs) = x
-- index2 (FS x) (y :: xs) = index2 k xs

index3 : (i:Fin n) -> (xs:Vect n a) -> a
index3 FZ (x :: xs) = x
index3 (FS FZ) (y :: xs) = index2 k xs
index3 (FS (FS x)) (y :: xs) = index3 k xs
