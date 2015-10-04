module Problem16 where

-- Problem 16
-- Drop every N'th element from a list.

dropEvery :: [a] -> Int -> [a]
dropEvery xs n
  = [x | (x, i) <- zip xs [1..], i `mod` n /= 0]
