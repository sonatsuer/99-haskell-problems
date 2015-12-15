module Problem96 where

import Data.Char
import qualified Data.Map as Map

-- Problem 96
-- Syntax checker

identifier :: String -> Bool
identifier [] = False
identifier (x : xs) =
  isAlpha x && rest xs
  where rest str = case str of
          [] ->
            True
          [c] ->
            isAlphaNum c
          '-' : c : cs  ->
            isAlpha c && rest cs
          c : cs ->
            rest cs
          _ ->
            False
          
