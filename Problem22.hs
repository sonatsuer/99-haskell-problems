module Problem22 where

-- Problem 22
-- Create a list containing all integers within a given range.

range :: Enum a => a -> a -> [a]
range = enumFromTo
