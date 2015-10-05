module Problem09 where 

import Data.List (group)

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. 

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x1 : x2 : xs) 
  | x1 == x2  = (x1 : head rest) : tail rest 
  | otherwise = [x1] : rest 
    where rest = pack (x2 : xs)


-- Using span.
pack2 :: Eq a => [a] -> [[a]]
pack2 [] = []
pack2 (x : xs) = l : pack2 r
  where (l, r) = span (==x) (x : xs)

-- One can also use the built in group function.

pack3 :: Eq a => [a] -> [[a]]
pack3 = group
