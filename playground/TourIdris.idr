module TourIdris

import Data.Vect
import IO

data Boolean = Verdadeiro | Falso

invert : Boolean -> Boolean
invert Verdadeiro = Falso
invert Falso = Verdadeiro

result1 : Boolean
result1 = invert Verdadeiro

-- AST
data Expr = Val Double
           | Multiply Expr Expr
           | Add Expr Expr
           | Subtract Expr Expr
           | Divide Expr Expr

total evaluate : Expr -> Double
evaluate (Val x) = x
evaluate (Multiply x y) = evaluate x * evaluate y
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Subtract x y) = evaluate x - evaluate y
evaluate (Divide x y) = evaluate x / evaluate y

-- Z => Zero
-- S => Sucessor
-- data Nat = Z | S Nat

-- Tyepe arguments
data LList elem = Nil | (::) elem (List elem)

takeList : (n : Nat) -> (list : List a) -> List a
takeList Z list = []
takeList (S k) [] = []
takeList (S k) (x :: xs) = x :: takeList k xs

-- dropList : (n : Nat) -> (list : List a) -> List a
-- dropList Z list = []
-- dropList (S k) [] = []
-- dropList (S k) (x :: xs) = dropList k xs

takeVect : (n : Nat) -> Vect (n + m) elem -> Vect n elem
takeVect Z xs = []
takeVect (S k) (x :: xs) = x :: takeVect k xs

-- using (x:a, y:a, xs:Vect n a)
--   data Elem : a -> Vect n a -> Type where
--     Here : Elem x (x :: xs)
--     There : Elem x xs -> Elem x (y :: xs)

-- putStr : String -> IO ()
--getLine : IO String

-- data File -- abstract
-- data Mode = Read | Write | ReadWrite

openFile : String -> Mode -> IO File
closeFile : File -> IO ()

fread : File -> IO String
fwrite : File -> String -> IO ()
feof : File -> IO Bool

readFile : String -> IO String

greet : IO ()
greet = do putStr "What is your name? "
           name <- getLine
           putStrLn ("Hello" ++ name)

boolCase : bool -> Lazy a -> Lazy a -> a
boolCase True t e = t
boolCase False t e = e

list_lookup : Nat -> List a -> Maybe a
list_lookup _ Nil = Nothing
list_lookup Z (x :: xs) = Just x
list_lookup (S k) (x :: s) = list_lookup k xs

fred : (String, Int)
fred = ("Fred", 42)

jim : (String, Int, String)
jim = ("Jim", 25, "Cambridge")

data Sigma : (A : Type) -> (P : A -> Type) -> Type where
  MkSigma : {P : A -> Type} -> (a:A) -> P a -> Sigma A P

vec : (n : Nat ** Vect n Int)
vec = (2 ** [3, 4])
-- equivalent to
vec : Sigma Nat (\n => Vect n Int)
vec = MkSigma 2 [3, 4]

vec : (n ** Vect n Int)
vec = (_ ** [3, 4])


filter : (a -> Bool) -> Vect n a -> (p ** Vect p a)
-- if Vect is empty
filter p Nil = (_ ** [])

filter p (x :: xs) with (filter p xs)
  | (_ ** xs') = if (p x) then (_ ** x :: x') else (_ ** xs')

intbounts : Int -> Int -> Bool
intbounts x y = x >= 0 && x < 640 && y >= 0 && y < 480

drawpoint : (x: Int) -> (y: Int) -> So (intbounts x y) -> IO ()
drawpoint x y p = unsafeDrawPoint x y

-- let binding
mirror : List a -> List a
mirror xs = lt xs' = reverse xs in xs ++ xs'

data Person = MkPeson String Int

showPerson : Person -> String
showPerson p = let MkPeson name age = p in name ++ " is " ++ show age ++ " years old"

pythag : Int -> List (Int, Int, Int)
pythag n [ (x,y,z) | z <- [1..n]],
                     y < [1..z],
                     x <- [1..y],
                     x*x + y*y == z*z ]

-- generator by list comprehention
[a,b..c] -- a - b == step in generator
