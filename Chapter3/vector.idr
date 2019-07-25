import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect k elem)) ->  Vect n (Vect (S k) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                              transposeHelper x xsTrans

transposeMat2 : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat2 [] = createEmpties
transposeMat2 (x :: xs) = let xs_trans = transposeMat2 xs in
                              zipWith (::) x xs_trans
