module Problem05 where

import DList

-- Problem 5
-- Reverse a list.


-- This solution uses an explicit accumulation parameter
-- and runs in linear time.

myReverse :: [a] -> [a]
myReverse xs = snd $ auxReverse (xs, [])
  where auxReverse ([], bs)      = ([], bs)
        auxReverse ( a : as, bs) = auxReverse (as, a : bs)


-- This is essentially the same algorithm but hides
-- the acummulation parameter in th Dlist module. 
-- It works slightly slower than myReverse but still 
-- in linear time.

myReverse2 :: [a] -> [a]
myReverse2 = toList . reversoToDList
  where reversoToDList []       = nilD
        reversoToDList (x : xs) = reversoToDList xs +++ singleton x
