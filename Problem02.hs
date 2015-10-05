module Problem02 where

-- Problem 2
-- Find the last but one element of a list.

myButLast :: [a] -> a
myButLast [x, _]   = x
myButLast (_ : xs) = myButLast xs
myButLast _        = error "There should be at least 2 items in the list."
