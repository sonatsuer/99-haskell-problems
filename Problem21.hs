module Problem21 where

-- Problem 21
-- Insert an element at a given position into a list.

insertAt :: a -> [a] -> Int -> [a]
insertAt x l n
  = if n <= 0 || n > 1 + length l then  error "Out of range."
    else  safeInsertAt x l n
          where safeInsertAt x [] _ 
                  = [x]
                safeInsertAt x (y : ys) 1
                  = x : y : ys
                safeInsertAt x (y : ys ) n
                  = y : safeInsertAt x ys (n - 1)
