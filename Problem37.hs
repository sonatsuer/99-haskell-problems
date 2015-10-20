module Problem37 where

import Problem36 (primeFactorsMult)

-- Problem 37
-- Calculate Euler's totient function phi(m) (improved).

phi :: Int -> Int
phi m
  | m <= 0    = error "Value should be positive."
  | otherwise = product [formula p k | (k, p) <- primeFactorsMult m]
    where formula x y = (x - 1)*(x^(y - 1))
