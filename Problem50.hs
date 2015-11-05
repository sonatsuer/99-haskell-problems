module Problem50 where

import Data.Ord (comparing)
import Data.List (sortBy)
-- Problem 50
-- Huffman Codes.

data Tree a = Node a | Branch (Tree a) (Tree a)
  deriving Show

constructTree :: [(a, Int)] -> Tree a
constructTree fs =assemble [(Node c, n) | (c,n) <- fs]

assemble :: [(Tree a, Int)] -> Tree a
assemble [] = error "List should be nonempty."
assemble [(t, _)] = t
assemble ts = assemble ((Branch t1 t2, n1 + n2) : rest)
  where ((t1, n1) : (t2, n2) : rest) = sortBy (comparing snd) ts

huffman' :: Tree a -> [(a, String)]
huffman' (Node x) = [(x,"")]
huffman' (Branch t1 t2) = left ++ right
  where left  = [(x, '0' : s) | (x, s) <- huffman' t1]
        right = [(x, '1' : s) | (x, s) <- huffman' t2]

huffman :: [(a, Int)] -> [(a, String)]
huffman = huffman' . constructTree
