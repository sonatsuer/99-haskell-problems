module Problem62b where

import Trees

-- Problem 62B
-- Collect the nodes at a given level in a list.

atLevel :: Int -> Tree a -> [a]
atLevel n Empty = []
atLevel n (Branch x t1 t2)
  | n == 1     = [x]
  | otherwise  = atLevel (n - 1) t1 ++ atLevel (n - 1) t2
