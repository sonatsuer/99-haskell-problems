module Problem71 where

import Trees

-- Problem 71.
-- Determine the internal path length of a tree.


internalPathLength :: MTree a -> Int
internalPathLength = aux 0
  where aux n (Node x ts) = n + sum  (map (aux (n + 1)) ts)
