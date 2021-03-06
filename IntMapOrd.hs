{-|
Module      : IntMapOrd
Description : Bidirectionalization
Copyright   : (c) Urska, 2015;
		          Melanija, 2015
License     : GPL-3

Stability   : experimental

-- A variant of the regular 'Data.IntMap'.
 -}
module IntMapOrd
	(IntMapOrd,
	fromAscPairList,
	empty, 
	insert,
	checkInsert, 
	union, 
	lookup, 
	lookupR,
	toAscList)
where

import qualified Data.Map.Strict as Map
import Prelude hiding (lookup)
import Data.Maybe
import Data.List as List hiding (insert, union, lookup, find)

newtype IntMapOrd b = IntMapOrd (Map.Map Int b) 

instance Show b => Show (IntMapOrd b) where
	show b =  show (toAscList b)

-- | Check if the keys in the map are in ascending order.
valid :: Ord b => IntMapOrd b -> Bool
valid (IntMapOrd map) = Map.valid map

-- | Check if the values in the map are in ascending order.
validR :: Ord b => IntMapOrd b -> Bool
validR map = 
	let s1 = List.map (\(_,y) -> y) (toAscList map)
	in 	if (check_list s1)
		then True
		else False
	
-- | Check if the values in the list are in ascending order.	
check_list :: Ord b => [b] -> Bool
check_list [] = True
check_list [x] = True
check_list (x:y:xs) = if x < y
					  then check_list (y:xs)
					  else False

-- | Build a map from an ascending list of key and value pairs.	
--   If the list if not ascending, an error is signalled.				  
fromAscPairList :: Ord b => [(Int, b)] -> IntMapOrd b
fromAscPairList list = 
	let 
		map1 = (List.map (\(x, y) -> x) list)
		map2 = (List.map (\(x, y) -> y) list)
	in 	if ((check_list map1) && (check_list map2))
		then ((IntMapOrd) (Map.fromAscList list))
		else error "Input list is not ascending."

-- | Convert to an ascending list.
toAscList :: IntMapOrd b -> [(Int, b)]
toAscList (IntMapOrd map) = Map.toAscList map

-- | The empty map.
empty :: IntMapOrd b
empty = (IntMapOrd) Map.empty

-- | Insert a new key and value pair in map. An error is signalled, 
--   if the order is violated.
insert :: (Ord b) => Int -> b -> IntMapOrd b -> IntMapOrd b
insert key value (IntMapOrd map) = 
	let 
		m1 = Map.insert key value map
	in 	
		if (valid (IntMapOrd m1)) && (validR (IntMapOrd m1))
		then (IntMapOrd) m1
		else error "Update violates order"

-- | Insert a new key and value pair in map. The key must be new or its value has 
--   to be equal to the present value, if not, an error is signalled.
--   An error is also signalled, if the order is violated.		
checkInsert :: Ord b => Int -> b -> IntMapOrd b -> Either String (IntMapOrd b)
checkInsert key value map = 
	case lookup key map of
		Nothing ->  case lookupR value map of 
						Nothing -> Right (insert key value map)
						Just m -> Left "Update violates equality"
		Just c  -> case lookupR value map of 
						Nothing -> Left "Update violates equality"
						Just a 	-> if key == a && value == c
								   then Right map
								   else Left "Update violates equality"

-- | The union of two maps. When duplicate keys are present, it prefers the first map.
--   An error is signalled, if the order is violated.								   
union :: Ord b => IntMapOrd b -> IntMapOrd b -> Either String (IntMapOrd b)
union (IntMapOrd map1) (IntMapOrd map2) = 
	let 
		s1 = Map.filterWithKey (\key _ -> Map.notMember key map1) (map2)
		s2 = Map.foldr (\value init -> (notMemberR value (IntMapOrd map1)) && init) True s1
		s3 = IntMapOrd (Map.union map1 s1)
	in
		if s2 && (valid s3) && (validR s3)
		then Right s3
		else Left "Update violates order"
		
-- | Check if the value is not a member of the map.			
notMemberR :: Ord b => b -> IntMapOrd b -> Bool
notMemberR value map = case lookupR value map of
					Nothing -> True
					Just _ 	-> False

-- | Lookup the value at a key in the map.					
lookup :: Ord b => Int -> IntMapOrd b -> Maybe b
lookup key (IntMapOrd map) = Map.lookup key map

-- | Lookup the key at a value in the map.
lookupR :: Ord b => b -> IntMapOrd b -> Maybe Int
lookupR value map = find value (toAscList map)

-- | Given a key find the value in a list of the key and value pairs.
find :: Ord b => b -> [(Int, b)] -> Maybe Int
find value [] = Nothing
find value ((x, y):xs) = if (y == value)
						 then Just (x)
						 else (find value xs)



	
