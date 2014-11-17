module IntMapOrd
	(fromAscPairList,
	empty, 
	insert,
	checkInsert, 
	union, 
	lookup, 
	lookupR,
	toAscList)
where

import qualified Data.Bimap as Bimap
import Prelude hiding (lookup)
import Data.Maybe

newtype IntMapOrd b = IntMapOrd (Bimap.Bimap Int b) 

instance Show b => Show (IntMapOrd b) where
	show b =  show (toAscList b)

fromAscPairList :: Ord b => [(Int, b)] -> IntMapOrd b
fromAscPairList list = IntMapOrd (Bimap.fromAscPairList list)

toAscList :: IntMapOrd b -> [(Int, b)]
toAscList (IntMapOrd bimap) = Bimap.toAscList bimap

empty :: IntMapOrd b
empty = IntMapOrd Bimap.empty

insert :: (Ord b) => Int -> b -> IntMapOrd b -> IntMapOrd b
insert key value (IntMapOrd bimap) = IntMapOrd (Bimap.tryInsert key value bimap)

checkInsert :: Ord b => Int -> b -> IntMapOrd b -> Either String (IntMapOrd b)
checkInsert key value bimap = undefined

union :: Ord b => IntMapOrd b -> IntMapOrd b -> Either String (IntMapOrd b)
union (IntMapOrd bimap1) (IntMapOrd bimap2) = undefined

lookup :: Ord b => Int -> IntMapOrd b -> Maybe b
lookup key (IntMapOrd bimap) = Bimap.lookup key bimap

lookupR :: Ord b => b -> IntMapOrd b -> Maybe Int
lookupR value (IntMapOrd bimap) = Bimap.lookupR value bimap



	