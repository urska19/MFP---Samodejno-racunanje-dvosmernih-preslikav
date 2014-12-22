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

valid :: Ord b => IntMapOrd b -> Bool
valid (IntMapOrd map) = Map.valid map

validR :: Ord b => IntMapOrd b -> Bool
validR map = 
	let s1 = List.map (\(_,y) -> y) (toAscList map)
	in 	if (check_list s1)
		then True
		else False
		
check_list :: Ord b => [b] -> Bool
check_list [] = True
check_list [x] = True
check_list (x:y:xs) = if x < y
					  then check_list (y:xs)
					  else False

fromAscPairList :: Ord b => [(Int, b)] -> IntMapOrd b
fromAscPairList list = 
	let 
		map1 = (List.map (\(x, y) -> x) list)
		map2 = (List.map (\(x, y) -> y) list)
	in 	if ((check_list map1) && (check_list map2))
		then ((IntMapOrd) (Map.fromAscList list))
		else error "Input list is not ascending."

toAscList :: IntMapOrd b -> [(Int, b)]
toAscList (IntMapOrd map) = Map.toAscList map

empty :: IntMapOrd b
empty = (IntMapOrd) Map.empty

insert :: (Ord b) => Int -> b -> IntMapOrd b -> IntMapOrd b
insert key value (IntMapOrd map) = 
	let 
		m1 = Map.insert key value map
	in 	
		if (valid (IntMapOrd m1)) && (validR (IntMapOrd m1))
		then (IntMapOrd) m1
		else error "Update violates equality"

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

union :: Ord b => IntMapOrd b -> IntMapOrd b -> Either String (IntMapOrd b)
union (IntMapOrd map1) (IntMapOrd map2) = 
	let 
		s1 = Map.filterWithKey (\key _ -> Map.notMember key map1) (map2)
		s2 = Map.foldr (\value init -> (notMemberR value (IntMapOrd map1)) && init) True s1
	in
		if s2 
		then Right (IntMapOrd (Map.union map1 s1))
		else Left "Vrednosti pri razlicnih kljucih so enake"
		
notMemberR :: Ord b => b -> IntMapOrd b -> Bool
notMemberR value map = case lookupR value map of
					Nothing -> True
					Just _ 	-> False

lookup :: Ord b => Int -> IntMapOrd b -> Maybe b
lookup key (IntMapOrd map) = Map.lookup key map

lookupR :: Ord b => b -> IntMapOrd b -> Maybe Int
lookupR value map = find value (toAscList map)

find :: Ord b => b -> [(Int, b)] -> Maybe Int
find value [] = Nothing
find value ((x, y):xs) = if (y == value)
						 then Just (x)
						 else (find value xs)



	
