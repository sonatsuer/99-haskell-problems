module Problem60 where

import Trees

-- Problem 60
-- Construct height-balanced binary trees with a given number of nodes.

-- Maximum number of nodes of a height balanced tree with
-- given height.
maxNodes :: Int -> Int
maxNodes h = 2^h - 1

-- Minimum Number of nodes of a binary tree with a 
-- given height.
minNodes :: Int -> Int
minNodes n = mnl !! n
  where mnl = 0 : 1 : zipWith ((+) . succ) mnl (tail mnl)

-- Maximum height of a height balanced tree with a given
-- number of nodes. 
maxHeight :: Int -> Int
maxHeight n = head [k | k <- [0..], minNodes (k + 1) > n]

-- Minimumimum height of a height balanced tree with a given
-- number of nodes. It is actually the logarithm function.
minHeight :: Int -> Int
minHeight n = head [k | k <- [0..], maxNodes k >= n]

-- All height balanced trees with given height and node number.
hbalTreeHN :: a -> Int -> Int -> [Tree a]
hbalTreeHN x 0 0 = [Empty]
hbalTreeHN x h n 
  | h > maxHeight n || h < minHeight n
     = []
  | otherwise
     =[Branch x t1 t2 | (hl, hr) <- [(h-1, h-1), (h-1, h-2), (h-2,h-1)],
                        k <- [0..n - 1],
                        t1 <- hbalTreeHN x hl k,
                        t2 <- hbalTreeHN x hr (n - k -1)] 

-- All height balanced trees with a given number of nodes.
hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n
  = [t | h <- [minHeight n..maxHeight n], t <- hbalTreeHN x h n]
