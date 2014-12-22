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

empty :: IntMapEq a
empty = IntMapEq (IntMap.empty)

insert :: Int -> a -> IntMapEq a -> IntMapEq a
insert key value (IntMapEq map) = IntMapEq (IntMap.insert key value map)

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

union :: Eq a => IntMapEq a -> IntMapEq a -> Either String (IntMapEq a)
union (IntMapEq map1) (IntMapEq map2) = 
	let 
		s1 = IntMap.filterWithKey (\key _ -> IntMap.notMember key map1) (map2)
		s2 = IntMap.foldr (\value init -> (notMemberR value (IntMapEq map1)) && init) True s1
	in
		if s2 
		then Right (IntMapEq (IntMap.union map1 s1))
		else Left "Vrednosti pri razlicnih kljucih so enake"

notMemberR :: Eq a => a -> IntMapEq a -> Bool
notMemberR value map = case lookupR value map of
					Nothing -> True
					Just _ 	-> False

lookup :: Int -> IntMapEq a -> Maybe a
lookup key (IntMapEq map) = IntMap.lookup key map

lookupR :: Eq a => a -> IntMapEq a -> Maybe Int
lookupR value map = find value (toList map)

find :: Eq a => a -> [(IntMap.Key, a)] -> Maybe Int
find value []  = Nothing
find value ((x, y):xs) = if (y == value)
						 then Just (x)
						 else (find value xs)

fromList :: Eq a => [(IntMap.Key, a)] -> IntMapEq a
fromList list = IntMapEq (IntMap.fromList list)

toList :: IntMapEq a -> [(IntMap.Key, a)]
toList (IntMapEq map) = IntMap.toList map
	
