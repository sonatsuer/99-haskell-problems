module Problem34 where

import Problem33 (coprime)

-- Problem 34
-- Calculate Euler's totient function phi(m).

totient :: Int -> Int
totient n | n <= 0    = error "Value should be positive"
          | otherwise = length [m | m <- [1..n], coprime m n]
