module KeymapTree ( Keymap (Leaf, Node),
                    invariant, keys,
                    size, depth, get, set, select,
                    toList, fromList,
                    filterLT, filterGT, merge,
                    -- for testing
                    prop_set_get, prop_toList_fromList,
                    testTree
                  )
where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)
                deriving (Eq, Show)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf
                               (Node 4 40 Leaf Leaf ))

-- Invariant

invariant :: Ord k => Keymap k a -> Bool
invariant Leaf  =  True
invariant (Node k _ left right)  =  all (< k) (keys left) &&
                                    all (> k) (keys right) &&
                                    invariant left &&
                                    invariant right

keys :: Keymap k a -> [k]
keys Leaf  =  []
keys (Node k _ left right)  =  keys left ++ [k] ++ keys right

size :: Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + (depth left `max` depth right)

-- Exercise 3

{-
Old and stupid version, flattens tree into unordered list and then sorts it
toList :: (Ord k) => Keymap k a -> [(k,a)]
toList tree = orderKVList (toListRec tree)

toListRec :: Keymap k a -> [(k,a)]
toListRec Leaf = []
toListRec (Node key val subtree1 subtree2) = (key,val) : toListRec subtree1 ++ toListRec subtree2

orderKVList :: (Ord k) => [(k,a)] -> [(k,a)]
orderKVList [] = []
orderKVList (first:rest) = orderKVList less ++ [first] ++ orderKVList more
            where
                less = [(k,v) | (k,v) <- rest, k<(fst(first))]
                more = [(k,v) | (k,v) <- rest, k>(fst(first))]
-}
-- New version, just use the fact that the tree is ordered!
toList :: (Ord k) => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node key val subtree1 subtree2) = toList subtree1 ++ [(key,val)] ++ toList subtree2


-- Exercise 4

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = go
    where go Leaf = Node key value Leaf Leaf
          go (Node k v left right) | key == k = Node k value left right
                                   | key < k  = Node k v (go left) right
                                   | key > k  = Node k v left (go right)


-- Exercise 5

get :: Ord k => k -> Keymap k a -> Maybe a
get key = go
    where
        go Leaf = Nothing
        go (Node k value left right)
                                    | key == k = Just value
                                    | key < k = go left
                                    | key > k = go right

prop_set_get :: Int -> String -> Keymap Int String -> Bool
prop_set_get k v db = get k (set k v db) == Just v

-- Exercise 6

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList listIn = foldr (\(k,v) tree -> set k v tree) Leaf listIn



prop_toList_fromList :: [Int] -> [String] -> Bool
prop_toList_fromList xs ys  =  toList (fromList zs) == sort zs
  where zs = zip (nub xs) ys


-- ** Optional Material

-- Exercise 8

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT _ Leaf = Leaf
filterLT k (Node key value left right)
                                    | key < k = Node key value left (filterLT k right)
                                    | key >= k = filterLT k left

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT _ Leaf =  Leaf
filterGT k (Node key value left right)
                                    | key >  k = Node key value (filterGT k left) right
                                    | key <= k = filterGT k right

-- Exercise 9

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge Leaf keymapB = keymapB
merge keymapA Leaf = keymapA
merge (Node keyA valA leftA rightA) t2 =
     Node keyA valA
          (merge leftA (filterLT keyA t2))
          (merge rightA (filterGT keyA t2))

-- Exercise 10

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select =  undefined

-- Instances for QuickCheck -----------------------------

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList (liftM2 zip (liftM nub arbitrary) arbitrary)
