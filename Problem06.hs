module Problem06 where

import Problem5 (myReverse)

-- Problem 6
-- Find out whether a list is a palindrome.

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs
