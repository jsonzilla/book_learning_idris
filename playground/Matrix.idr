import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat [] = createEmpties
transpose_mat (x :: xs) = let xs_trans = transpose_mat xs in zipWith (::) x xs_trans

f : Vect m (Vect n elem) -> Vect n (Vect m elem)
f [] = createEmpties
f (x :: xs) = let g = f xs in zipWith (::) x g

--addMatrix_rhs_1 : Vect (S len) (Vect m a)

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

app : Vect n a -> Vect m a -> Vect (n + m) a
app [] [] = []
app [] ys = ys
app (x :: xs) ys = x :: app xs ys


multMatrix : Num a => Vect m (Vect n a) -> Vect n (Vect p a) -> Vect m (Vect p a)
