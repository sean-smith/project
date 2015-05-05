module Interpret where

import AbstractSyntax
import KeyValueStore
import TypeCheck

makeSet :: Value -> Value
makeSet Error      = Error
makeSet (Number n) = Set [n]
makeSet (Set ns)   = Set ns

type KeyValueStore = [([String], Value)]
type Algorithm = KeyValueStore -> Maybe KeyValueStore

eval :: [(String, KeyValueStore)] -> Exp -> Algorithm
-- Complete for Problem 3, part (d).

exec :: [(String, KeyValueStore)] -> Stmt -> Algorithm
-- Complete for Problem 3, part (d).

typeCheckInterpret :: Stmt -> Algorithm
-- Complete for Problem 3, part (e).

--eof