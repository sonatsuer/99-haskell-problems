module Problem36 where

import Problem35 (primeFactors)
import Problem10 (encode)

-- Problem 36
-- Determine the prime factors of a given positive integer, second version

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = encode . primeFactors
