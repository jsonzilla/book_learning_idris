module TourIdris

import Data.Vect

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
