module Problem31 where

-- Problem 31
-- Determine whether a given integer number is prime.

-- Uses the more mathematical definition, namely "p is prime if
-- and only if any decomposition into a product uses a unit."
isPrime :: Int -> Bool
isPrime n
  | (-2) < n && n < 2   = False
  | n == 2 || n == (-2) = True
  | even n              = False
  | otherwise           = and [n `mod ` i  /= 0 | i <- [3, 5..(bound n)]]
  where bound = floor . sqrt . fromIntegral . abs
