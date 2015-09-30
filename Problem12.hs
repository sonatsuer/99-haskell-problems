module Problem12 where

import Multiplicity

-- Problem 12
-- Inverse of encodeModified

decodeModified :: Eq a => [MuSi a] -> [a]
decodeModified [] = []
decodeModified (Multiple n x : xs)
  | n == 2    = [x, x] ++ decodeModified xs
  | otherwise = x : decodeModified (Multiple (n - 1) x : xs)
decodeModified (Single x : xs)
  = x : decodeModified xs
