module Problem59 where

import Trees


-- Problem 59
-- Construct height-balanced binary trees.


hbalTree :: Int -> a -> [Tree a]
hbalTree 0 _ = [Empty]
hbalTree 1 x = [Branch x Empty Empty]
hbalTree n x = [Branch x t1 t2 | t1 <- prev, t2 <- prev]
               ++
               [Branch x t1 t2 | t1 <- pprev, t2 <- prev]
               ++
               [Branch x t1 t2 | t1 <- prev, t2 <- pprev]
  where prev  = hbalTree (n - 1) x
        pprev = hbalTree (n - 2) x
