module Problem19 where

-- Problem 19
-- Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n 
  = drop reduced xs ++ take reduced xs
    where reduced = n `mod` myLength xs
