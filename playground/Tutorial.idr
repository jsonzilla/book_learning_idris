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

-- index3 : (i:Fin n) -> (xs:Vect n a) -> a
-- index3 FZ (x :: xs) = x
-- index3 (FS FZ) (y :: xs) = index2 k xs
-- index3 (FS (FS x)) (y :: xs) = index3 k xs

pythag : Int -> List (Int, Int, Int)
pythag n = [ (x,y,z) | z <- [1..n], y <-[1..z], x <- [1..y] , x * x + y * y == z * z]

splitAt : Char -> String -> (String, String)
splitAt c x = case break (== c) x of
                  (x, y) => (x, strTail y)

-- lookup_default : Nat -> List a -> a -> a
-- lookup_default i xs def = case list_lookup i xs of
--                               Nothing => def
--                               Just x => x

-- record Person : Type where
--   MkPerson : (name: String) -> (age: Int) -> Person
--
-- fred : Person
-- fred = MkPerson "Fred" 30

-- record Class : Type where
--   ClassInfo : (students : Vect n Person) ->
--         (className : String) ->
--         Class

interface Show2 a where
  show_me : a -> String

implementation Show2 Nat where
  show_me Z = "Z"
  show_me (S k) = "s" ++ show_me k

implementation Show2 a => Show2 (Vect n a) where
  show_me xs = "[" ++ show' xs ++ "]" where
    show' : Vect n a -> String
    show' Nil = ""
    show' (x :: Nil) = show_me x
    show' (x :: xs) = show_me x ++ ", " ++ show' xs

sortAndShow : (Ord a, Show a) => List a -> String
sortAndShow xs = show (sort xs)

-- m_add : Maybe Int -> Maybe Int -> Maybe Int
-- m_add x y = return (!x + !y)

m_add2 : Maybe Int -> Maybe Int -> Maybe Int
m_add2 x y = [x' + y' | x' <- x, y' <- y]

m_app : Maybe (a -> b) -> Maybe a -> Maybe b
m_app (Just f) (Just a) = Just (f a)
m_app _ _ = Nothing

-- m_add' : x Maybe Int -> Maybe Int -> Maybe Int
-- m_add' x y = m_app (m_app (Just (+)) x) y

infixl 2 <&*>
interface Applicative (f: Type -> Type) where
  pure : a -> f a
  (<&*>) : f (a -> b) -> f a -> f b

-- implementation Applicative Maybe where
--   pure = Just
--   (Just f) <&*> (Just a) = Just (f a)
--   _ <&*> _ = Nothing

  -- m_add : Maybe Int -> Maybe Int -> Maybe Int
  -- m_add x y = return (!x + !y)
