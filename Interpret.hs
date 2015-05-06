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

eval env (Min e) = liftMaybe suffix . liftMaybe (combine 1 min) . (eval env e)
eval env (Max e) = liftMaybe suffix . liftMaybe (combine 1 max) . (eval env e)
eval env (Sum e) = liftMaybe suffix . liftMaybe (combine 1 (+)) . (eval env e)
eval env (Product e) = liftMaybe suffix . liftMaybe (combine 1 (*)) . (eval env e)
eval env (Union e) = liftMaybe suffix . liftMaybe (combine 1 (\/)) . (eval env e)
eval env (Intersection e) = liftMaybe suffix . liftMaybe (combine 1 (/\)) . (eval env e)
eval env DATA = (\s -> Just(s)) 
eval env (Variable x) = (\s -> lookup x env)
eval env (MakeSet e) = liftMaybe (\s -> [ (x, makeSet y)  | (x, y) <- s]) . (eval env e)


exec :: [(String, KeyValueStore)] -> Stmt -> Algorithm
exec env (Return x) = (\s -> lookup x env)

exec env (Assign x e s) kvs = 
  let a = (eval env e) kvs
  in if a == Nothing then Nothing else 
    exec (env++[(x, (justValue a))]) s kvs



typeCheckInterpret :: Stmt -> Algorithm
-- Complete for Problem 3, part (e).
typeCheckInterpret ast kvs = 
  let t = typeCheck [] ast
  in if t /= Nothing && justValue t == TyVoid then 
    exec [] ast kvs
  else Nothing

--eof
