module Problem61 where

import Trees

--Problem 61
-- Count the leaves of a binary tree.

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch x Empty Empty) = 1
countLeaves (Branch x t1 t2) = countLeaves t1 + countLeaves t2
