module IntMapEq
	(empty, 
	insert, 
	checkInsert, 
	union, 
	lookup, 
	lookupR)
where

import Prelude hiding (lookup)
import qualified Data.IntMap as IntMap
import Data.Maybe

newtype IntMapEq a = IntMapEq (IntMap.IntMap a) deriving Show

empty :: IntMapEq a
empty = IntMapEq (IntMap.empty)

insert :: Int -> a -> IntMapEq a -> IntMapEq a
insert key value (IntMapEq map) = IntMapEq (IntMap.insert key value map)

checkInsert :: Eq a => Int -> a -> IntMapEq a -> Either String (IntMapEq a)
checkInsert key value (IntMapEq map) = 
	case lookup key (IntMapEq map) of
		Nothing -> Right (IntMapEq (IntMap.insert key value map))
		Just c  -> if value == c
				   then Right (IntMapEq map)
				   else Left "Update violates equality"

union :: Eq a => IntMapEq a -> IntMapEq a -> Either String (IntMapEq a)
union (IntMapEq m1) (IntMapEq m2) = Right (IntMapEq (IntMap.union m1 m2))

lookup :: Int -> IntMapEq a -> Maybe a
lookup key (IntMapEq map) = IntMap.lookup key map

lookupR :: Eq a => a -> IntMapEq a -> Maybe Int
lookupR = undefined
{-lookupR value (IntMapEq map) = case map of
	empty -> Nothing
	Tip k v -> if v==value	
			   then Just k
			   else Nothing
	Bin _ _ m1 m2-> let 
						k1 = lookupR value m1
						k2 = lookupR value m2
					 in
						if (Data.Maybe.isJust k1)
						then k1
						else if (Data.Maybe.isJust k2)
						then k2
						else Nothing
-}

	