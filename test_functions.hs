{-|
Module      : test_functions
Description : Bidirectionalization
Copyright   : (c) Urska, 2015;
		  Melanija, 2015
License     : GPL-3

Stability   : experimental

-- Test functions for automatic bidirectionalization.
-}

import Bff
import Data.List 
import Data.Maybe (fromJust)
 
-- |Simple tree type.
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- | Flatens tree data type into list.
flatten :: Tree a -> [a]
flatten (Leaf a) = [a]
flatten (Node t1 t2 ) = flatten t1 ++ flatten t2

-- | Gives first three elts in ordered list without duplicates.
top3 :: Ord a => [a] -> [a]
top3 = take 3 . Data.List.sort . Data.List.nub

-- | Returns every other elt in a list.
sieve :: [a] -> [a]
sieve (a : b : cs) = b : sieve cs
sieve _ = []

-- | Removes duplicates in a list.
rmdups :: Eq a => [a] -> [a]
rmdups = Data.List.nub

-- | Returns first half of a list.
halve :: [a] -> [a]
halve s = take (length s `div` 2) s

-- | Put function of halve.
put1 :: [a] -> [a] -> [a]
put1 xs xs' | length xs' == n
    = xs' ++ drop n xs
      where n = length xs `div` 2

-- | Put function of flatten.
put2 :: Tree a -> [a] -> Tree a
put2 s v = case go s v of (t, [ ]) -> t
  where go (Leaf a) (b : bs) = (Leaf b, bs)
	go (Node s1 s2) bs = (Node t1 t2, ds)
	  where (t1, cs) = go s1 bs
		(t2, ds) = go s2 cs
		
-- | Put function of rmdups.
put3 :: Eq a => [a] -> [a] -> [a]
put3 s v | v == Data.List.nub v && length v == length s'
	= map (fromJust . flip lookup (zip s' v)) s
	where s' = Data.List.nub s
  
