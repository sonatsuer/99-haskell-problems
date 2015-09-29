module Problem11 where

import Problem10 (encode)
import Multiplicity

-- Problem 11
-- Modified run-length encoding.

-- If an element has no duplicates it is simply copied into
-- the result list. Since Haskell lists are homogeneous we
-- have to use a different data type, Multiple-Single lists
-- from the Multiplicity module.
           
encodeModified :: Eq a => [a] -> [MuSi a]
encodeModified xss = map format (encode xss)
  where format (1, x) = Single x
        format (n, x) = Multiple n x
  
