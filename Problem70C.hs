module Problem70c where

import Trees

-- Problem 70s
-- Count the nodes of a multiway tree.

nOfNodes :: MTree a -> Int
nOfNodes = mTreeFold f
  where f _ ns = 1 + sum ns
