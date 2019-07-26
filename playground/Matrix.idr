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

-- leftmatrix n m rigthMatrix m p 1 transpose

multV : Num a => (xs : Vect n a) -> (ys : Vect n a) -> a
multV xs ys = sum (zipWith (*) xs ys)

createRow : Num a => (x : Vect n a) -> (ysTranspose : Vect p (Vect n a)) -> Vect p a
createRow x [] = []
createRow x (y :: ys) = multV x y :: createRow x ys
-- mkRow x (y :: xs) = multVecs x y :: mkRow x xs

multMatrix_rhs : Num a => (xs : Vect m (Vect n a)) -> (ysTranspose : Vect p (Vect n a)) -> Vect m (Vect p a)
multMatrix_rhs [] ysTranspose = []
multMatrix_rhs (x :: xs) ysTranspose = createRow x ysTranspose :: multMatrix_rhs xs ysTranspose

multMatrix : Num a => Vect m (Vect n a) -> Vect n (Vect p a) -> Vect m (Vect p a)
multMatrix xs ys = let ysTranspose = transpose_mat ys in
                        multMatrix_rhs xs ysTranspose
