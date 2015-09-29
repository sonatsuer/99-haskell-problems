module Problem3 where

-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt [] _       = error "Position does not exist."
elementAt (x : _)  1 = x
elementAt (_ : xs) n = elementAt xs (n - 1)
