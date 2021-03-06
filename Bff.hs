{-|
Module      : Bff
Description : Bidirectionalization
Copyright   : (c) Urska, 2015;
		  Melanija, 2015
License     : GPL-3

Stability   : experimental

-- Bff module contains automatic bidirectionalizer (by the article Bidirectionalization for Free!, Janis Voigtlaender).
-}

{-# OPTIONS_GHC -XRank2Types #-}

module Bff where
	
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap (fromAscList, empty, notMember,insert, union, lookup)
import IntMapEq 
import qualified IntMapEq as IntMapEq (empty, insert, checkInsert, union, lookup, lookupR)
import IntMapOrd (IntMapOrd)
import qualified IntMapOrd as IntMapOrd (union, lookup, fromAscPairList, empty, checkInsert, lookupR)
import Data.Maybe (fromJust)
import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State (get, put)
import Control.Applicative
import Control.Functor.Combinators.Lift
import Data.Set (Set)
import qualified Data.Set as Set (toAscList, singleton)
import Data.Traversable
import Data.Foldable
import Zippable


-- | bff, given a polymorphic get function, source and updated view, returns  
--   the updated source (automatically produces corresponding put).
bff :: (Traversable k, Zippable k', Foldable k') => (forall a. k a -> k' a) -> (forall a. Eq a => k a -> k' a -> k a)	
bff get = \s v ->
  let (s', g) = template s
      h = either error id (assoc (get s') v)
      h' = IntMap.union h g
  in seq h (fmap (fromJust . flip IntMap.lookup h') s')
    
-- | Produces template for source when using bff.  
template :: Traversable k => k a -> (k Int, IntMap a) 
template s = 
  case runState (go s)([],0)
       of (s', (l, _)) -> (s', IntMap.fromAscList (reverse l))
  where go = unwrapMonad . traverse (WrapMonad . number)
 
-- | Describes the action to be performed for every element found in a source.
number :: a -> State ([(Int, a)], Int) Int
number a = do (l, i) <- State.get 
	      State.put ((i, a) : l, i+1)
	      return i

-- | Determines the matching between template view and the updated proper value view,
assoc :: (Zippable k, Foldable k, Eq a) => k Int -> k a -> Either String (IntMap a)
assoc = makeAssoc Bff.checkInsert IntMap.empty

makeAssoc checkInsert empty s'' v =
  either Left f (tryZip s'' v)
    where f = Data.Foldable.foldr 
                (either Left . uncurry checkInsert) 
                (Right empty) 

checkInsert :: Eq a => Int -> a -> IntMap a -> Either String (IntMap a)
checkInsert x y m = case IntMap.lookup x m of
			 Nothing -> Right (IntMap.insert x y m)
			 Just c -> if y == c 
				      then Right m 
				      else Left "Update violates equality."
							      

							      
-- | Similar as bff function, but can also handle get functions that compare the elements.
bff_Eq :: (Traversable k, Zippable k', Foldable k') => (forall a. Eq a => k a -> k' a) -> (forall a. Eq a => k a -> k' a -> k a)
bff_Eq get = \s v ->
   let (s',g) = template_Eq s
       h = either error id (assoc_Eq(get s') v)
       h' = either error id (IntMapEq.union h g)
   in seq h' (fmap (fromJust . flip IntMapEq.lookup h') s')
   
-- | Produces template for source when using bff_Eq.     
template_Eq :: (Traversable k, Eq a) => k a -> (k Int, IntMapEq a)
template_Eq s = case runState (go s) (IntMapEq.empty,0)
		  of (s',(g,_)) -> (s',g)
  where go = unwrapMonad . traverse (WrapMonad . number_Eq)

-- | Describes the action to be performed for every element found in a source, 
--   and by which integer key to replace it in the template.
number_Eq :: Eq a => a -> State (IntMapEq a, Int) Int
number_Eq a = do (m,i) <- State.get
		 case IntMapEq.lookupR a m of
		   Just j -> return j
		   Nothing -> do let m' = IntMapEq.insert i a m
				 State.put (m',i+1)
				 return i

-- | Determines the matching between template view and the updated proper value view,
assoc_Eq :: (Zippable k, Foldable k, Eq a) => k Int -> k a -> Either String (IntMapEq a)
assoc_Eq = makeAssoc IntMapEq.checkInsert IntMapEq.empty
  


-- | Similar as bff function, but can also handle get functions that compare the elements
--  using 'Ord' typeclass.
bff_Ord :: (Traversable k, Zippable k', Foldable k') => (forall a. Ord a => k a -> k' a) -> (forall a. Ord a => k a -> k' a -> k a)
bff_Ord get = \s v ->
   let (s',g) = template_Ord s
       h = either error id (assoc_Ord(get s') v)
       h' = either error id (IntMapOrd.union h g)
   in seq h' (fmap (fromJust . flip IntMapOrd.lookup h') s')   

-- | Produces template for source when using bff_Ord.     
template_Ord :: (Traversable k, Ord a) => k a -> (k Int, IntMapOrd a)
template_Ord s = case traverse number_Ord s of 
		      Lift (Const xs, f) -> let m = set2map xs in (f m,m)

-- | Describes the action to be performed for every element found in a source, 
--   and by which integer key to replace it in the template.		      
number_Ord :: Ord a => a -> Lift(,)(Const(Set a))((->)(IntMapOrd a)) Int 
number_Ord a = Lift (Const(Set.singleton a), fromJust . IntMapOrd.lookupR a)

-- | Determines the matching between template view and the updated proper value view,
assoc_Ord :: (Zippable k, Foldable k, Ord a) => k Int -> k a -> Either String (IntMapOrd a)
assoc_Ord = makeAssoc IntMapOrd.checkInsert IntMapOrd.empty

-- | Helps to build proper template in case of using bff_Ord
set2map :: Ord a => Set a -> IntMapOrd a
set2map xs = IntMapOrd.fromAscPairList (zip[0..] (Set.toAscList xs))
