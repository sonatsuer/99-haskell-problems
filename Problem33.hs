module Problem33 where

import Problem32 (myGCD)

-- Problem 33
-- Determine whether two positive integer numbers are coprime.

coprime :: Int -> Int -> Bool
coprime x y = myGCD x y == 1
