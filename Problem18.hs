module Problem18 where

-- Problem 18
-- Extract a slice from a list.

slice :: [a] -> Int -> Int -> [a]
slice xs m n
  = [x | (x, i) <- zip xs [1..], m <= i && i <= n]
