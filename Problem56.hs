module Problem56 where

import Trees

-- Problem 56
-- Symmetric Binary Trees.

symmetric :: Eq a => Tree a -> Bool
symmetric t = t == mirror t
  where mirror    = treeFold f Empty
        f x t1 t2 = Branch x t2 t1
