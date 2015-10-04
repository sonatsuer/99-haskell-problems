module Problem13 where

import Multiplicity

-- Problem 13
-- Run-length encoding of a list (direct solution).

encodeDirect :: Eq a => [a] -> [MuSi a]
encodeDirect = foldr combine []
  where combine :: Eq a => a -> [MuSi a] -> [MuSi a]
        combine x1 [] = [Single x1]
        combine x1 (Single x2 : xs)
          | x1 == x2  = Multiple 2 x1 : xs
          | x1 /= x2  = Single x1 : Single x2 : xs
        combine x1 (Multiple n x2 : xs)
          | x1 == x2  = Multiple (n + 1) x1 : xs
          | x1 /= x2  = Single x1 : Multiple n x2 : xs
