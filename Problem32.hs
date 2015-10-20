module Problem32 where

-- Problem 32
-- Determine the greatest common divisor of two positive 
-- integer numbers. Use Euclid's algorithm.

myGCD :: Int -> Int -> Int
myGCD m n 
  = myGCD'(abs m) (abs n)
    where myGCD' m 0 = m
          myGCD' m n = myGCD' n (m `mod` n)
