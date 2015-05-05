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
-- for when n == 1
-- v1 = [DATA, (Variable "x"), (Variable "y")]
-- for when n > 1
-- v2 = [Max h, Min h, Sum h, Product h, Union h, Intersection h, MakeSet h]
  exhaustive n = if n == 1 then [DATA, (Variable "x"), (Variable "y")] else [Max h |  h  <- (exhaustive ((n-1)))]++[Min h |  h  <- (exhaustive (n-1))]++[Sum h |  h  <- (exhaustive (n-1))]++[Product h |  h  <- (exhaustive (n-1))]++[Union h |  h  <- (exhaustive (n-1))]++[Intersection h |  h  <- (exhaustive (n-1))]++[MakeSet h |  h  <- (exhaustive (n-1))]

instance Exhaustive Stmt where
  -- Complete for Problem 4, part (a).
  exhaustive n = if n == 1 then [(Return "x"), (Return "y")] else [Assign "x" e s |  s  <- ((exhaustive (n-1))::[Stmt]), e  <- ((exhaustive (n-1))::[Exp])]++[Assign "y" e s |  s  <- ((exhaustive (n-1))::[Stmt]), e  <- ((exhaustive (n-1))::[Exp])]


--f:: Stmt -> KeyValueStore -> Maybe KeyValueStore
--g:: Stmt -> KeyValueStore -> Maybe KeyValueStore


validate :: Integer -> (Stmt -> Algorithm) -> (Stmt -> Algorithm) -> [KeyValueStore] -> [(Stmt, KeyValueStore)]
-- Complete for Problem 4, part (b).
-- type Algorithm = KeyValueStore -> Maybe KeyValueStore
-- n f g kvs
validate n f g kvs = [ (x,v) | x <- (exhaustive n), v <- kvs,  (f x v) /= (g x v)    ]


--eof
