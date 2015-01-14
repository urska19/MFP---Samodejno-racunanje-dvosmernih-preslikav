{-|
Module      : IntMapEq
Description : Bidirectionalization
Copyright   : (c) Urska, 2015;
		          Melanija, 2015
License     : GPL-3

Stability   : experimental

-- A variant of the regular 'Data.IntMap'.
-}

module IntMapEq
	(IntMapEq,
	empty, 
	insert, 
	checkInsert, 
	union, 
	notMemberR,
	lookup, 
	lookupR,
	fromList,
	toList)
where

import Prelude hiding (lookup)
import qualified Data.IntMap as IntMap
import Data.Maybe

newtype IntMapEq a = IntMapEq (IntMap.IntMap a) 

instance Show a => Show (IntMapEq a) where
	show a =  show (toList a)


-- | The empty map.
empty :: IntMapEq a
empty = IntMapEq (IntMap.empty)

-- | Insert a new key and value pair in the map. If the key is already present in the map, 
--   the associated value is replaced with the supplied value.
insert :: Int -> a -> IntMapEq a -> IntMapEq a
insert key value (IntMapEq map) = IntMapEq (IntMap.insert key value map)

-- | Insert a new key and value pair in map. The key must be new or its value has 
--   to be equal to the present value, if not, an error is signalled.
checkInsert :: Eq a => Int -> a -> IntMapEq a -> Either String (IntMapEq a)
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
--   An error is signalled, if values at different keys are equal.							
union :: Eq a => IntMapEq a -> IntMapEq a -> Either String (IntMapEq a)
union (IntMapEq map1) (IntMapEq map2) = 
	let 
		s1 = IntMap.filterWithKey (\key _ -> IntMap.notMember key map1) (map2)
		s2 = IntMap.foldr (\value init -> (notMemberR value (IntMapEq map1)) && init) True s1
	in
		if s2 
		then Right (IntMapEq (IntMap.union map1 s1))
		else Left "Values at different keys are equal"

-- | Check if the value is not a member of the map.		
notMemberR :: Eq a => a -> IntMapEq a -> Bool
notMemberR value map = case lookupR value map of
					Nothing -> True
					Just _ 	-> False

-- | Lookup the value at a key in the map.					
lookup :: Int -> IntMapEq a -> Maybe a
lookup key (IntMapEq map) = IntMap.lookup key map

-- | Lookup the key at a value in the map.
lookupR :: Eq a => a -> IntMapEq a -> Maybe Int
lookupR value map = find value (toList map)

-- | Given a key find the value in a list of the key and value pairs.
find :: Eq a => a -> [(IntMap.Key, a)] -> Maybe Int
find value []  = Nothing
find value ((x, y):xs) = if (y == value)
						 then Just (x)
						 else (find value xs)

-- | Create a map from a list of key and value pairs.						 
fromList :: Eq a => [(IntMap.Key, a)] -> IntMapEq a
fromList list = IntMapEq (IntMap.fromList list)

-- | Convert the map to a list of key and value pairs.
toList :: IntMapEq a -> [(IntMap.Key, a)]
toList (IntMapEq map) = IntMap.toList map
	
