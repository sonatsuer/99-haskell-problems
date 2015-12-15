module Problem95 where

import Data.List (intercalate)

-- Problem 95
-- English words.

fullWords :: Int -> String
fullWords n =
  intercalate "-" [name k | k <- show n]


name :: Char -> String
name c =
  case c of
    '0' -> "zero"
    '1' -> "one"
    '2' -> "two"
    '3' -> "three"
    '4' -> "four"
    '5' -> "five"
    '6' -> "six"
    '7' -> "seven"
    '8' -> "eight"
    '9' -> "nine"
    _   -> error "Not a numeral!"
