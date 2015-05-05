module Validate where

import AbstractSyntax
import Interpret

-- Functions for generating key-value stores for testing
-- purposes given a collection of integers and keys.
dat :: [[String]] -> [Integer] -> KeyValueStore
dat (ks:kss) ns = [(k:ks', n) | (ks', n) <- dat kss ns, k <- ks]
dat []       ns = [([], Number n) | n <- ns]

dats :: [[String]] -> [Integer] -> [KeyValueStore]
dats kss ns = [dat (rotate i kss) (rotate j ns) | i <- [0..length kss-1], j <- [0..length ns-1]]
  where rotate i xs = take (length xs) (drop i (cycle xs))

class Exhaustive a where
  exhaustive :: Integer -> [a]

instance Exhaustive Exp where
  -- Complete for Problem 4, part (a).

instance Exhaustive Stmt where
  -- Complete for Problem 4, part (a).

validate :: Integer -> (Stmt -> Algorithm) -> (Stmt -> Algorithm) -> [KeyValueStore] -> [(Stmt, KeyValueStore)]
-- Complete for Problem 4, part (b).

--eof