module Problem35 where

-- Problem 35
-- Determine the prime factors of a given positive integer.

-- The algortihm depends on the observations that a minimum positive
-- divisor of an integer is necessarily prime.
primeFactors :: Int -> [Int]
primeFactors m
  | m <= 0    = error "Value should be positive."
  | m == 1    = []
  | otherwise = p : primeFactors (m `div` p)
    where p = head [n | n <- [2..m], m `mod` n == 0]
