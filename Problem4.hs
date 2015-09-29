module Problem4 where

-- Problem 4
-- Find the number of elements of a list.

myLength :: [a] -> Int
myLength []       = 0
myLength (x : xs) = 1 + myLength xs


-- Better, using foldr.

myLength2 :: [a] -> Int
myLength2 = foldr (\x y -> 1 + y) 0 
