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
-- combine n ops kvs 
eval env (Min e) kvs = 
  let a = (eval env e kvs)
  in if a == Nothing then Nothing else 
    Just (suffix (combine 1 (\x y -> min x y) (justValue a)))
eval env (Max e) kvs = 
  let a = (eval env e kvs)
  in if a == Nothing then Nothing else 
    Just (suffix (combine 1 (\x y -> max x y) (justValue a)))
eval env (Sum e) kvs = 
  let a = (eval env e kvs)
  in if a == Nothing then Nothing else 
    Just (suffix (combine 1 (\x y -> x + y) (justValue a)))
eval env (Product e) kvs = 
  let a = (eval env e kvs)
  in if a == Nothing then Nothing else 
    Just (suffix (combine 1 (\x y -> x * y) (justValue a)))
eval env (Union e) kvs =
  let a = (eval env e kvs)
  in if a == Nothing then Nothing else 
    Just (suffix (combine 1 (\x y -> (\/) x y) (justValue a)))  
eval env (Intersection e) kvs = 
  let a = (eval env e kvs)
  in if a == Nothing then Nothing else 
    Just (suffix (combine 1 (\x y -> (/\) x y) (justValue a)))
eval env DATA kvs = Just(kvs)
eval env (Variable x) kvs = 
  let l = filter (\v -> (fst v) == x) env 
    in if length l == 0 then Nothing else
      Just(snd (l!!0))
eval env (MakeSet e) kvs = 
  let a = (eval env e kvs)
  in if a == Nothing then Nothing else 
    Just([   (y,  (makeSet x))    |   (y, x)   <- (justValue a) ])



exec :: [(String, KeyValueStore)] -> Stmt -> Algorithm
-- I DONT LIKE THIS
exec env (Return x) kvs = 
  let a = (eval env (Variable x) kvs)
  in if a == Nothing then Nothing else 
    a
exec env (Assign x e s) kvs = 
  let a = (eval env e kvs)
  in if a == Nothing then Nothing else 
    let b = exec (env++[(x, (justValue a))]) s kvs
    in if b == Nothing then Nothing else
      b


typeCheckInterpret :: Stmt -> Algorithm
-- Complete for Problem 3, part (e).
typeCheckInterpret ast kvs = 
  let t = typeCheck [] ast
  in if t /= Nothing && justValue t == TyVoid then 
    exec [] ast kvs
  else Nothing

--eof
