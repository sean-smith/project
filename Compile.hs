module Compile where

import Data.List (union, (\\))
import AbstractSyntax
import TypeCheck
import Interpret
import KeyValueStore

data Instruction =
    Reliable (Value -> Value -> Value)
  | Unreliable (Value -> Value -> Value)
  | MakeSets
  | DropKeySuffixes

-- If a variable assignment statement will never affect the
-- final returned key-value store at the bottom of the abstract
-- syntax tree, remove that statement.
rmvDeadCode :: Stmt -> Stmt
rmvDeadCode (Assign x e s) = if not (x `elem` unbound (rmvDeadCode s)) then (rmvDeadCode s) else Assign x e (rmvDeadCode s)
rmvDeadCode (Return x    ) = Return x

unbound :: Stmt -> [String]
unbound (Assign x e s) = vars (Assign "" e (Return "")) `union` (unbound s \\ [x])
unbound (Return x    ) = [x]

compileExp :: Exp -> [Instruction]
compileExp (Max e          ) = compileExp e ++ [Reliable max]
compileExp (Min e          ) = compileExp e ++ [Reliable min]
compileExp (Sum e          ) = compileExp e ++ [Reliable (+)]
compileExp (Product e      ) = compileExp e ++ [Reliable (*)]
compileExp (Union e        ) = compileExp e ++ [Reliable (\/)]
compileExp (Intersection e ) = compileExp e ++ [Reliable (/\)]
compileExp (MakeSet e      ) = compileExp e ++ [MakeSets]
compileExp _                 = []

compileStmt :: Stmt -> [Instruction]
compileStmt (Assign _ e s) = compileExp e ++ compileStmt s
compileStmt (Return _    ) = []

simulate :: [Instruction] -> KeyValueStore ->  KeyValueStore
simulate ((Reliable f)   :insts) kvs = simulate insts (combine 1 f kvs)
simulate ((Unreliable f) :insts) kvs = simulate insts (combine 2 f kvs)
simulate (DropKeySuffixes:insts) kvs = simulate insts (suffix kvs)
simulate (MakeSets       :insts) kvs = simulate insts [(x, makeSet y)  | (x, y) <- kvs]

compile :: Stmt -> KeyValueStore -> Maybe KeyValueStore
-- Complete for Problem 5, part (b).
-- Compile should first type check the program, then should use rmvDeadCode
-- to eliminate any dead code, and finally should compile the program and 
-- apply the simulator to it to obtain a function on key-value stores 
-- that can be compared to the interpreter.
compile s kvs =  let t = typeCheck [] s in if justValue t /= TyVoid then Nothing else Just(simulate (compileStmt (rmvDeadCode s)) kvs)

--eof