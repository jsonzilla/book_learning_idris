module Palindrome

export
palindrome : Nat -> String -> Bool
palindrome size str = toLower str == toLower (reverse str) && isLong str
    where
      isLong : String -> Bool
      isLong str = size < (length str)
