module Trees where

-- Definition of the binary tree used in the examples
-- together with its fold.

data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Eq, Show)

treeFold :: (a -> b -> b -> b) -> b -> Tree a -> b
treeFold _ x Empty = x
treeFold f x (Branch y t1 t2)
  = f y (treeFold f x t1) (treeFold f x t2) 



-- Definition of the multiway tree used in the examples
-- together with its fold.
data MTree a = Node a [MTree a]
  deriving (Eq, Show)

mTreeFold :: (a -> [b] -> b) -> MTree a -> b
mTreeFold f (Node x ts) =
  f x $ map (mTreeFold f) ts
