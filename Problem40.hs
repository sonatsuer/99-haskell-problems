module Problem40 where

import Problem31 (isPrime)
import Problem39 (primesR)

-- Problem 40
-- Goldbach's conjecture.

goldbach :: Int -> (Int, Int)
goldbach m
  | m < 4     = error "Value should be at least 4."
  | m == 4    = (2, 2)
  | odd m     = error "Value should be even."
  | otherwise = head [(p, m - p) | p <- primesR 3 m, isPrime (m - p)]
