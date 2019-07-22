mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = not (isEven k)

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k
