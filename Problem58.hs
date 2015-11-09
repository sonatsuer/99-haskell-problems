module Problem58 where

import Trees
import Problem55
import Problem56

-- Problem 58
-- Generate-and-test paradigm.

symCbalTrees :: Int -> [Tree Char]
symCbalTrees = (filter symmetric) . cbalTrees
