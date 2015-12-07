module MiniParse where

import Control.Applicative
import Control.Monad(ap)



-- Definition and instances of a simple monadic parser. Failure
-- is coded by the empty list in nondeterministic parsing
-- and by Nothing in deterministic parsing.
----------------------------------------------------------------------

newtype Parser a = P (String -> [ (a, String) ])

-- Non-determinisitc parse. This is what makes Parser a monad.
ndParse :: Parser a -> String -> [ (a, String) ]
ndParse (P f) s = f s

-- Forced deterministic parse. Discards the return value. 
parse :: Parser a -> String -> Maybe a
parse p s = case ndParse p s of
            (x, _) : _ -> Just x 
            _          -> Nothing

instance Monad Parser where
  return x = P $ \ s -> [ (x, s) ]

  p >>= pf = P $ \ s -> do (x, s) <- ndParse p s
                           ndParse (pf x) s

  fail _   = P $ const []

instance Applicative Parser where  
   pure   = return
   (<*>)  = ap

instance Functor Parser where
   fmap = liftA

instance Alternative Parser where
  empty = P $ const []

  p1 <|> p2 = P $ \s -> case (ndParse p1 s, ndParse p2 s) of
                             (x : _, _) -> [x]
                             (_, y : _) -> [y]
                             _          -> []
                        



-- Some useful parsers and parser combinators.
----------------------------------------------------------------------

-- Reads one charcter.
getCh :: Parser Char
getCh = P $ \ s -> case s of
                     ""     -> []
                     c : cs -> [(c, cs)]


-- Succeeds only on the empty string.
end :: Parser ()
end = P $ \ s -> case s of
                 "" -> [((), "")]
                 _  -> []


-- final p succeeds only if p consumes all the input.
final :: Parser a -> Parser a
final p = do x <- p
             end
             return x


-- Picks a single character satisfying the predicate, fails otherwise.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do c <- getCh
               if (f c) then return c else empty


-- Picks the given character, fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)


-- This is nondeterministic choice. Combines all possible
-- results. Note that <|> is deterministic.
choose :: Parser a -> Parser a -> Parser a
choose p1 p2 = P (\ s -> ndParse p1 s ++ ndParse p2 s)
