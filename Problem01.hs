module Problem01 where

-- Problem 1
-- Find the last element of a list.

myLast :: [a] -> a
myLast [x]      = x
myLast (_ : xs) = myLast xs
myLast _        = error "The list should be nonempty."
