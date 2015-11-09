module Trees where

-- Definition of the tree used in the examples.
-- Also a few usefull functions on trees.

data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Eq, Show)

treeFold :: (a -> b -> b -> b) -> b -> Tree a -> b
treeFold _ x Empty = x
treeFold f x (Branch y t1 t2)
  = f y (treeFold f x t1) (treeFold f x t2) 
