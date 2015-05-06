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
-- NOT SURE IF IT SHOULD BE ZERO
-- combine n ops kvs 

eval env (Min e) = liftMaybe suffix . liftMaybe (combine 1 min) . (eval env e)
eval env (Max e) = liftMaybe suffix . liftMaybe (combine 1 min) . (eval env e)
eval env (Sum e) = liftMaybe suffix . liftMaybe (combine 1 min) . (eval env e)
eval env (Product e) = liftMaybe suffix . liftMaybe (combine 1 min) . (eval env e)
eval env (Union e) = liftMaybe suffix . liftMaybe (combine 1 min) . (eval env e)
eval env (Intersection e) = liftMaybe suffix . liftMaybe (combine 1 min) . (eval env e)





--  let a = (eval env e kvs)
--  in if a == Nothing then Nothing else 
--    (liftMaybe suffix (liftMaybe (combine 1 min) a))
--eval env (Max e) kvs = 
--  let a = (eval env e kvs)
--  in if a == Nothing then Nothing else 
--    (liftMaybe suffix (liftMaybe (combine 1 max) a))

--eval env (Sum e) kvs = 
--  let a = (eval env e kvs)
--  in if a == Nothing then Nothing else 
--    (liftMaybe suffix (liftMaybe (combine 1 (+)) a))
--eval env (Product e) kvs = 
--  let a = (eval env e kvs)
--  in if a == Nothing then Nothing else 
--    (liftMaybe suffix (liftMaybe (combine 1 (*)) a))
--eval env (Intersection e) kvs = 
--  let a = (eval env e kvs)
--  in if a == Nothing then Nothing else 
--    (liftMaybe suffix (liftMaybe (combine 1 (/\)) a))
--eval env (Union e) kvs = 
--  let a = (eval env e kvs)
--  in if a == Nothing then Nothing else 
--    (liftMaybe suffix (liftMaybe (combine 1 (\/)) a))
--eval env DATA kvs = Just(kvs)
--eval env (Variable x) kvs = 
--  let a = [snd y | y <- env, fst y == x] 
--  in if length a > 0 then Just(a!!0) else Nothing

--eval env (MakeSet e) kvs = 
--  let a = (eval env e kvs)
--  in if a == Nothing then Nothing else 
--    Just([    (y,makeSet x)    |   (y, x)   <- (justValue a) ])



exec :: [(String, KeyValueStore)] -> Stmt -> Algorithm
-- I DONT LIKE THIS
-- Need to clarify the inference rules
exec env (Return x) kvs = 
  let a = [snd y | y <- env, fst y == x] 
  in if length a > 0 then Just(a!!0) else Nothing

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
