module Zippable  where

import Control.Applicative


class Zippable k where
  tryZip :: k a -> k b -> Either String (k (a,b))

instance Zippable [] where
  tryZip [] [] = Right []
  tryZip (x:xs) (y:ys) = Right (:) <*> Right (x,y)
				    <*> tryZip xs ys
  tryZip _ _ = Left "Update changes the length."
  
--instance Zippable Maybe where
--  tryZip (Just a) (Just b) = Right (Just (a b))
--  tryZip _ _ = Right Nothing

