module Problem61a where

import Trees
import Dlist

-- Problem 61a
-- Collect the leaves of a binary tree in a list.

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch x t1 t2) = leaves t1 ++ leaves t2

-- Here is a difference list version.

leavesD :: Tree a -> DList a
leaves Empty = nilD
leaves (Branch x Empty Empty) = singleton x
leaves (Branch x t1 t2) = leaves t1 +++ leaves t2
