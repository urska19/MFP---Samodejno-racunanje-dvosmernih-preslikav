module Bff where
	
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap (fromAscList, empty, notMember,insert, union, lookup)
import Data.Maybe (fromJust)
--import IntMapEq 
--import qualified IntMapEq as IntMapEq (empty, insert, checkInsert, union, lookup, lookupR)
import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State (get, put)
import Data.List -- za primere
	
	

bff :: (forall a. [a] -> [a]) -> (forall a. Eq a => [a] -> [a] -> [a])
bff get = \s v ->
  let s' = [0..(length s) - 1]
      g = IntMap.fromAscList (zip s' s)
      h = either error id (assoc (get s') v)
      h' = IntMap.union h g
  in seq h (map (fromJust . flip IntMap.lookup h') s')
  
assoc :: Eq a => [Int] -> [a] -> Either String (IntMap a)
assoc [] [] = Right IntMap.empty
assoc (x:xs) (y:ys) = either Left (Bff.checkInsert x y)(assoc xs ys) 
assoc _ _ = Left "Update changes the length."

checkInsert :: Eq a => Int -> a -> IntMap a -> Either String (IntMap a)
checkInsert x y m = case IntMap.lookup x m of
			 Nothing -> Right (IntMap.insert x y m)
			 Just c -> if y == c 
				      then Right m 
				      else Left "Update violates equality."
		      
		      
		      
template_Eq :: Eq a => [a] -> ([Int], IntMapEq a)
template_Eq s = case runState (go s) (IntMapEq.empty,0)
		  of (s',(g,_)) -> (s',g)
	where go [] = return []
	      go (x:xs) = do i <- number_Eq x
			     is <- go xs
			     return (i:is)
			      
number_Eq :: Eq a => a -> State (IntMapEq a, Int) Int
number_Eq a = do (m,i) <- State.get
		 case IntMapEq.lookupR a m of
		   Just j -> return j
		   Nothing -> do let m' = IntMapEq.insert i a m
				 State.put (m',i+1)
				 return i
			      
assoc_Eq :: Eq a => [Int] -> [a] -> Either String (IntMapEq a)
assoc_Eq [] [] = Right IntMapEq.empty
assoc_Eq (x:xs) (y:ys) = either Left (IntMapEq.checkInsert x y) (assoc_Eq xs ys)
assoc_Eq _ _ = Left "Update changes the length."

bff_Eq :: (forall a. Eq a => [a] -> [a]) -> (forall a. Eq a => [a] -> [a] -> [a])
bff_Eq get = \s v ->
   let (s',g) = template_Eq s
       h = either error id (assoc_Eq(get s') v)
       h' = either error id (IntMapEq.union h g)
   in seq h' (map (fromJust . flip IntMapEq.lookup h') s')

   
--za primere		      
sieve :: [a] -> [a]
sieve (a : b : cs) = b : sieve cs
sieve _ = []

rmdups :: Eq a => [a] -> [a]
rmdups = Data.List.nub
