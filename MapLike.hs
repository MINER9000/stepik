module MapLike where

import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap []
    lookup k empty                 = Nothing
    lookup k (ListMap ((x, y):xs)) | x == k    = Just y
                                   | otherwise = lookup k (ListMap xs)
    insert k v empty    = ListMap [(k, v)]
    insert k v (ListMap [(x, y)]) | x == k = ListMap [(k, v)]
                                  | x >  k = ListMap [(k, v), (x, y)]
                                  | x <  k = ListMap [(x, y), (k, v)]
    insert k v (ListMap l@(p1:p2:xs))
        | k <  fst p1 = ListMap $ (k, v) : l
        | k == fst p1 = ListMap $ (k, v) : p2 : xs
        | k <  fst p2 = ListMap $ p1 : (k, v) : p2 : xs
        | k == fst p2 = ListMap $ p1 : (k, v) : xs
        | k >  fst p2 = ListMap (p1:p2:getListMap(insert k v (ListMap xs)))
    delete k empty = empty
    delete k (ListMap ((x, y):xs))
        | k == x    = ListMap $ xs
        | otherwise = ListMap ((x, y) : getListMap (delete k (ListMap xs)))
