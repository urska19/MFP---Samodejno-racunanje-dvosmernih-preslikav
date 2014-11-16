{-# OPTIONS_GHC -XRank2Types #-}

module Bff where
	
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap (fromAscList, empty, notMember,insert, union, lookup)
--
import IntMapEq 
import qualified IntMapEq as IntMapEq (empty, insert, checkInsert, union, lookup, lookupR)
import IntMapOrd (IntMapOrd)
import qualified IntMapOrd as IntMapOrd (union, lookup, fromAscPairList, empty, checkInsert, lookupR)
--
import Data.Maybe (fromJust)
import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State (get, put)
import Control.Applicative
import Control.Functor.Combinators.Lift
import Data.Set (Set)
import qualified Data.Set as Set (toAscList, singleton)
import Data.Traversable
import Data.Foldable


-- za primere
import Data.List 

bff :: (Traversable k, Zippable k', Foldable k') => (forall a. k a -> k' a) -> (forall a. Eq a => k a -> k' a -> k a)	
bff get = \s v ->
  let (s', g) = template s
      h = either error id (assoc (get s') v)
      h' = IntMap.union h g
  in seq h (fmap (fromJust . flip IntMap.lookup h') s')
  
template :: Traversable k => k a -> (k Int, IntMap a) 
template s = 
  case runState (go s)([],0)
       of (s', (l, _)) -> (s', IntMap.fromAscList (reverse l))
  where go = unwrapMonad . traverse (WrapMonad . number)
 
number :: a -> State ([(Int, a)], Int) Int
number a = do (l, i) <- State.get 
	      State.put ((i, a) : l, i+1)
	      return i

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
							      

template_Eq :: (Traversable k, Eq a) => k a -> (k Int, IntMapEq a)
template_Eq s = case runState (go s) (IntMapEq.empty,0)
		  of (s',(g,_)) -> (s',g)
  where go = unwrapMonad . traverse (WrapMonad . number_Eq)

number_Eq :: Eq a => a -> State (IntMapEq a, Int) Int
number_Eq a = do (m,i) <- State.get
		 case IntMapEq.lookupR a m of
		   Just j -> return j
		   Nothing -> do let m' = IntMapEq.insert i a m
				 State.put (m',i+1)
				 return i

assoc_Eq :: (Zippable k, Foldable k, Eq a) => k Int -> k a -> Either String (IntMapEq a)
assoc_Eq = makeAssoc IntMapEq.checkInsert IntMapEq.empty
  
bff_Eq :: (Traversable k, Zippable k', Foldable k') => (forall a. Eq a => k a -> k' a) -> (forall a. Eq a => k a -> k' a -> k a)
bff_Eq get = \s v ->
   let (s',g) = template_Eq s
       h = either error id (assoc_Eq(get s') v)
       h' = either error id (IntMapEq.union h g)
   in seq h' (fmap (fromJust . flip IntMapEq.lookup h') s')

-- --------- 
-- ---------
collect :: Ord a => [a] -> Const(Set a) [b]
collect s = traverse(\a -> Const(Set.singleton a)) s
   
--traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
--traverse f [] = pure []
--traverse f (x:xs) = pure (:) <*> f x <*> Bff.traverse f xs   
   
propagate :: Ord a => [a] -> ((->) (IntMapOrd a)) [Int]
propagate s = traverse (\a -> fromJust . IntMapOrd.lookupR a) s   
   
--template_Ord :: Ord a => [a] -> ([Int], IntMapOrd a)
template_Ord :: (Traversable k, Ord a) => k a -> (k Int, IntMapOrd a)
template_Ord s = case traverse number_Ord s of 
		      Lift (Const xs, f) -> let m = set2map xs in (f m,m)
				  
number_Ord :: Ord a => a -> Lift(,)(Const(Set a))((->)(IntMapOrd a)) Int 
number_Ord a = Lift (Const(Set.singleton a), fromJust . IntMapOrd.lookupR a)

assoc_Ord :: (Zippable k, Foldable k, Ord a) => k Int -> k a -> Either String (IntMapOrd a)
assoc_Ord = makeAssoc IntMapOrd.checkInsert IntMapOrd.empty

bff_Ord :: (Traversable k, Zippable k', Foldable k') => (forall a. Ord a => k a -> k' a) -> (forall a. Ord a => k a -> k' a -> k a)
bff_Ord get = \s v ->
   let (s',g) = template_Ord s
       h = either error id (assoc_Ord(get s') v)
       h' = either error id (IntMapOrd.union h g)
   in seq h' (fmap (fromJust . flip IntMapOrd.lookup h') s')   

 
-- ------------------
-- ####
class Zippable k where
  tryZip :: k a -> k b -> Either String (k (a,b))

instance Zippable [] where
  tryZip [] [] = Right []
  tryZip (x:xs) (y:ys) = Right (:) <*> Right (x,y)
				    <*> tryZip xs ys
  tryZip _ _ = Left "Update changes the length."

  
instance Zippable Tree where
  tryZip (Leaf x) (Leaf y) = Right (Leaf(x,y))
  tryZip (Node t1 t2) (Node v1 v2) = Right Node 
				      <*> tryZip t1 v1
				      <*> tryZip t2 v2
  tryZip _ _ = Left "Update changes the shape."  
-- ###
-- ---------------

--za primere
data Tree a = Leaf a | Node (Tree a) (Tree a)

flatten :: Tree a -> [a]
flatten (Leaf a) = [a]
flatten (Node t1 t2 ) = flatten t1 ++ flatten t2

--instance Traversable Tree where
--traverse f (Leaf a) = pure Leaf <*> f a
--traverse f (Node t1 t2 ) = pure Node <*> traverse f t1
--				      <*> traverse f t2


top3 :: Ord a => [a] -> [a]
top3 = take 3 . Data.List.sort . Data.List.nub

set2map :: Ord a => Set a -> IntMapOrd a
set2map xs = IntMapOrd.fromAscPairList (zip[0..] (Set.toAscList xs))

sieve :: [a] -> [a]
sieve (a : b : cs) = b : sieve cs
sieve _ = []

rmdups :: Eq a => [a] -> [a]
rmdups = Data.List.nub
