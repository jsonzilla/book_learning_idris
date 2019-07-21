module Main

import Palindrome

showResult : Bool -> String
showResult result = if result == True
                    then "\nIs palindrome\n"
                    else "\nIs not palindrome\n"

showPalindrome : String -> String
showPalindrome str = showResult (palindrome 10 str)

main : IO()
main = repl "Enter a string: " showPalindrome
