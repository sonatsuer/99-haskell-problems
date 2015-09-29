module DList where

-- This is a simple difference list module to
-- have constant time concatenation.

newtype DList a = DList { act :: [a] -> [a] }

singleton :: a -> DList a
singleton x = DList { act = \ l -> x : l }

nilD :: DList a
nilD = DList { act = id }

(+++) :: DList a -> DList a -> DList a
l1 +++ l2 = DList { act = act l1 . act l2 }

toList :: DList a -> [a]
toList l = act l []
