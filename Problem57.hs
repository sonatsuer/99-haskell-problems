module Problem57 where

import Trees

-- Problem 57
-- Binary search trees, not necessarily balanced.

construct :: Ord a => [a] -> Tree a
construct = foldr insert Empty

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Branch x Empty Empty
insert x (Branch y t1 t2)
  | x < y  = Branch y (insert x t1) t2
  | x >= y = Branch y t1 (insert x t2)
