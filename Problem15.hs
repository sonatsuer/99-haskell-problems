module Problem15 where

-- Problem 15
-- Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs
