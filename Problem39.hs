module Problem39 where

import Problem31 (isPrime)

-- Problem 39
-- A list of prime numbers.

primesR :: Int -> Int -> [Int]
primesR x y = [p | p <- [x..y], isPrime p]
