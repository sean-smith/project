module KeyValueStore where

import Data.List (sort)

-- A function for finding two elements in a key-value store
-- that have the same key.
--
--  * The first argument is the key-value store, assumed
--    to already be sorted so pairs with the same key
--    are contiguous.
--  * The second argument is an accumulator list that
--    will maintain k the list entries that have no
--    partners with duplicate keys (thus, they must be kept
--    in the key-value store).
--  * If no duplicate keys are found, the result is Nothing;
--    otherwise, the result will be a tuple containing the
--    key, the two values under that key, and a new key-value
--    store that contains everything from the original one
--    except the two entries corresponding to the two values.

twoWithSameKeyHelper :: (Ord a, Ord b, Eq a) => [(a,b)] -> [(a,b)] -> Maybe (a, b, b, [(a,b)])
-- Complete recursive case for Problem 3, part (a).
twoWithSameKeyHelper ((k1,v1):(k2,v2):kvs) keep  = if k1 == k2 then Just(k1, v1, v2, keep++kvs) else twoWithSameKeyHelper kvs (keep++[(k1,v1),(k2,v2)])
twoWithSameKeyHelper _  _  = Nothing


--twoWithSameKey, which will behave exactly like twoWithSameKeyHelper 
--but will not require an initial accumulator variable to be specified, and does not assume 
--that the input list is `sorted.`
twoWithSameKey :: (Ord a, Ord b, Eq a) => [(a,b)] -> Maybe (a, b, b, [(a,b)])
-- Complete for Problem 3, part (b).
-- (\x -> fst x)
-- NOT SURE HOW TO SORT WITH A KEY SO IT PROBABLY WON'T WORK
twoWithSameKey kvs = twoWithSameKeyHelper (sort kvs) [] 

suffix :: [([a],b)] -> [([a],b)]
-- Complete for Problem 3, part (c).
suffix kvs = [(ms, x) | (m:ms, x) <- kvs]

-- A function for simulating the application of an operation to
-- a key-value store.
--
--  n* The first argument specifies how many duplicates of the 
--    result of each pair-wise operation will be added to the
--    resulting database.
--  op* The second argument is the operation to be applied to
--    all pairs of entries that have the same key.
--  kvs* The third argument is the key-value store itself.
--  * The result is the new key-value store in which all pairs
--    with identical keys have been combined.

combine :: (Ord a, Ord b, Eq a) =>  Int -> (b -> b -> b) -> [([a],b)] -> [([a],b)]
combine n op kvs =
  case twoWithSameKey kvs of
    Nothing -> kvs
    Just (k, v1, v2, kvs') -> combine n op (take n (repeat (k, op v1 v2)) ++ kvs')

--eof