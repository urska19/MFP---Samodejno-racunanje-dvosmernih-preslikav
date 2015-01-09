{-|
Module      : Zippable
Description : Bidirectionalization
Copyright   : (c) Urska, 2015;
		  Melanija, 2015
License     : GPL-3

Stability   : experimental

-- Class of data structures that can match equal shapes and combine the values.
-}

module Zippable
      (Zippable,
      tryZip)
where

import Control.Applicative

class Zippable k where
  tryZip :: k a -> k b -> Either String (k (a,b))

instance Zippable [] where
  tryZip [] [] = Right []
  tryZip (x:xs) (y:ys) = Right (:) <*> Right (x,y)
				    <*> tryZip xs ys
  tryZip _ _ = Left "Update changes the length."
  
data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Zippable Tree where
  tryZip (Leaf x) (Leaf y) = Right (Leaf (x,y))
  tryZip (Node n m) (Node o p) = Right Node 
				    <*> tryZip n o
				    <*> tryZip m p
  tryZip _ _ = Left "Update chances the shape"
 
instance Zippable Maybe where
  tryZip (Just a) (Just b) = Right (Just (a, b))
  tryZip _ _ = Right Nothing
