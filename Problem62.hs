module Problem62 where

import Trees
import DList

-- Problem 62
-- Collect the internal nodes of a binary tree in a list.

internals :: Tree a -> [a]
internals Empty = []
internals (Branch x Empty Empty) = []
internals (Branch x t1 t2) = internals t1 ++ [x] ++ internals t2

-- Again, here is a difference list version.

-- Problem 62
-- Collect the internal nodes of a binary tree in a list.

internalsD :: Tree a -> DList a
internals Empty = nilD
internals (Branch x Empty Empty) = nilD
internals (Branch x t1 t2)
  = internals t1 +++ singleton x +++ internals t2
