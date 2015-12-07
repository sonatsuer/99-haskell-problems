module Problem68 where

import Trees
import DList
import Data.List ((\\))

-- Problem 68
-- Preorder and inorder sequences of binary trees.

preorder :: Tree Char -> String
preorder = toList . aux
  where aux t =
          case t of
            Empty ->
              nilD
            Branch x t1 t2 ->
              singleton x +++ aux t1 +++ aux t2

inorder :: Tree Char -> String
inorder= toList . aux
  where aux t =
          case t of
            Empty ->
              nilD
            Branch x t1 t2 ->
              aux t1 +++ singleton x +++ aux t2


separateAt :: Char -> String -> (String, String)
separateAt x xs = (l, r)
  where (l, _ : r) = span (/=x) xs


-- Here the first argumet is the preorder sequence and
-- the second is the inorder.
preInTree :: String -> String -> Tree Char
preInTree [] _ = Empty
preInTree (x : xs) ys
  = Branch x (preInTree po1 io1) (preInTree po2 io2)
    where (io1, io2) = separateAt x ys
          l = length io1
          po1 = take l xs 
          po2 = drop l xs
