module Problem63 where

import Trees

-- Problem 63
-- Construct a complete binary tree.

-- This produces a tree of height h with 2^h - 1 nodes.
full :: a -> Int -> Tree a
full _ 0 = Empty
full x h = Branch x (full x (h -1)) (full x (h -1))

-- This is the maximum height of a full tree
-- that can be built from at most n nodes.
maxHeightFull :: Int -> Int
maxHeightFull n = head [k | k <- [0..], 2^(k + 1) - 1> n]

-- Constructs a complete binary with a given number of nodes.
cbt :: a -> Int -> Tree a
cbt x n 
  | d == 0          = full x h
  | d < 2^(h - 1)   = Branch x (cbt x (2^(h - 1) -1 + d)) (full x (h - 1))
  | otherwise       = Branch x (full x h) (cbt x (d - 1))
    where h = maxHeightFull n
          d = n - 2^h + 1
