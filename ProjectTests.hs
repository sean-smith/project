module ProjectTests where

import AbstractSyntax
import Parse
import TypeCheck
import KeyValueStore
import Interpret
import Validate
import Compile

allTests = [
  show (failed parseTests),
  show (failed typeCheckTests),
  show (failed interpretTests)
  ]

-- To get the failures for an individual test, query that
-- test using "failed", e.g.:
-- 
-- *> failed parseTests

failed :: Eq a => [(a, a)] -> [(a, a)]
failed tests = [(x, y) | (x, y) <- tests, x /= y]

parseTests :: [(Maybe Stmt, Maybe Stmt)]
parseTests = [
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = union(set(DATA)); return y;"), Just (Assign "y" (Union (MakeSet DATA)) (Return "y"))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = union(set(DATA)); assign y = y; return y;"), Just (Assign "y" (Union (MakeSet DATA)) (Assign "y" (Variable "y") (Return "y")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = union(set(DATA)); assign y = set(DATA); return y;"), Just (Assign "y" (Union (MakeSet DATA)) (Assign "y" (MakeSet DATA) (Return "y")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = union(set(DATA)); assign y = DATA; return y;"), Just (Assign "y" (Union (MakeSet DATA)) (Assign "y" DATA (Return "y")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = union(set(DATA)); assign x = y; return y;"), Just (Assign "y" (Union (MakeSet DATA)) (Assign "x" (Variable "y") (Return "y")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = union(set(DATA)); assign x = y; return x;"), Just (Assign "y" (Union (MakeSet DATA)) (Assign "x" (Variable "y") (Return "x")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = union(set(DATA)); assign x = set(DATA); return y;"), Just (Assign "y" (Union (MakeSet DATA)) (Assign "x" (MakeSet DATA) (Return "y")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = union(set(DATA)); assign x = set(DATA); return x;"), Just (Assign "y" (Union (MakeSet DATA)) (Assign "x" (MakeSet DATA) (Return "x")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = union(set(DATA)); assign x = DATA; return y;"), Just (Assign "y" (Union (MakeSet DATA)) (Assign "x" DATA (Return "y")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = union(set(DATA)); assign x = DATA; return x;"), Just (Assign "y" (Union (MakeSet DATA)) (Assign "x" DATA (Return "x")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = sum(DATA); return y;"), Just (Assign "y" (Sum DATA) (Return "y"))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = sum(DATA); assign y = y; return y;"), Just (Assign "y" (Sum DATA) (Assign "y" (Variable "y") (Return "y")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = sum(DATA); assign y = set(DATA); return y;"), Just (Assign "y" (Sum DATA) (Assign "y" (MakeSet DATA) (Return "y")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = sum(DATA); assign y = DATA; return y;"), Just (Assign "y" (Sum DATA) (Assign "y" DATA (Return "y")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = sum(DATA); assign x = y; return y;"), Just (Assign "y" (Sum DATA) (Assign "x" (Variable "y") (Return "y")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = sum(product(DATA)); assign y = product(DATA); return y;"), Just (Assign "y" (Sum (Product DATA)) (Assign "y" (Product DATA) (Return "y")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = sum(product(DATA)); assign y = product(DATA); assign y = y; return y;"), Just (Assign "y" (Sum (Product DATA)) (Assign "y" (Product DATA) (Assign "y" (Variable "y") (Return "y"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = sum(product(DATA)); assign y = product(DATA); assign y = set(DATA); return y;"), Just (Assign "y" (Sum (Product DATA)) (Assign "y" (Product DATA) (Assign "y" (MakeSet DATA) (Return "y"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = sum(product(DATA)); assign y = product(DATA); assign y = DATA; return y;"), Just (Assign "y" (Sum (Product DATA)) (Assign "y" (Product DATA) (Assign "y" DATA (Return "y"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = sum(product(DATA)); assign y = product(DATA); assign x = y; return y;"), Just (Assign "y" (Sum (Product DATA)) (Assign "y" (Product DATA) (Assign "x" (Variable "y") (Return "y"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = set(DATA); return y;"), Just (Assign "y" (MakeSet DATA) (Return "y"))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = set(DATA); assign y = y; return y;"), Just (Assign "y" (MakeSet DATA) (Assign "y" (Variable "y") (Return "y")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = set(DATA); assign y = set(DATA); return y;"), Just (Assign "y" (MakeSet DATA) (Assign "y" (MakeSet DATA) (Return "y")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = set(DATA); assign y = DATA; return y;"), Just (Assign "y" (MakeSet DATA) (Assign "y" DATA (Return "y")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign y = union(set(DATA)); assign y = DATA; return y;"), Just (Assign "y" (Union (MakeSet DATA)) (Assign "y" DATA (Return "y"))))
  ]

typeCheckTests :: [(Maybe Type, Maybe Type)]
typeCheckTests = [
  ((typeCheck [] :: Stmt -> Maybe Type) (Assign "a" (Sum DATA) (Assign "b" (Variable "a") (Return "a"))), Just TyVoid),
  ((typeCheck [] :: Stmt -> Maybe Type) (Assign "a" (Sum DATA) (Assign "b" (Variable "a") (Return "b"))), Just TyVoid),
  ((typeCheck [] :: Stmt -> Maybe Type) (Assign "b" (Variable "a") (Return "b")), Nothing),
  ((typeCheck [] :: Stmt -> Maybe Type) (Assign "a" (Sum DATA) (Assign "a" DATA (Return "a"))), Just TyVoid),
  ((typeCheck [] :: Stmt -> Maybe Type) (Assign "a" (Variable "b") (Return "a")), Nothing),
  ((typeCheck [] :: Exp -> Maybe Type) (Product (Product (Product (Product (Product (Min (Product DATA))))))), Just TyNumber),
  ((typeCheck [] :: Stmt -> Maybe Type) (Assign "p" (Variable "q") (Return "p")), Nothing),
  ((typeCheck [] :: Stmt -> Maybe Type) (Assign "x" (Sum (Product DATA)) (Assign "y" (Intersection (Intersection (MakeSet (Variable "x")))) (Return "y"))), Just TyVoid),
  ((typeCheck [] :: Stmt -> Maybe Type) (Assign "q" DATA (Return "q")), Just TyVoid),
  ((typeCheck [] :: Exp -> Maybe Type) (Product (Max (Product DATA))), Just TyNumber),
  ((typeCheck [] :: Exp -> Maybe Type) (Product (Sum (Min (Product (Sum (Product (Product DATA))))))), Just TyNumber),
  ((typeCheck [] :: Exp -> Maybe Type) (Product (Product (Max (Product (Sum (Product (Product DATA))))))), Just TyNumber),
  ((typeCheck [] :: Exp -> Maybe Type) (Product (Sum (Max (Product (Sum (Product (Product DATA))))))), Just TyNumber)
  ]

interpretTests :: [(Maybe KeyValueStore, Maybe KeyValueStore)]
interpretTests = [
  (typeCheckInterpret (Assign "x" (Min (Max DATA)) (Assign "y" (Intersection (Intersection (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [5]),([],Set [5])]),
  (typeCheckInterpret (Assign "x" (Min (Max DATA)) (Assign "y" (Union (Union (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [5]),([],Set [5])]),
  (typeCheckInterpret (Assign "a" (Min (Product DATA)) (Assign "b" (Union (Union (MakeSet (Variable "a")))) (Return "b"))) testKVS, Just [([],Set [-30]),([],Set [-30])]),
  (typeCheckInterpret (Assign "a" (Max (Sum DATA)) (Assign "b" (Intersection (Intersection (MakeSet (Variable "a")))) (Return "b"))) testKVS, Just [([],Set [9]),([],Set [9])]),
  (typeCheckInterpret (Assign "a" (Max (Sum DATA)) (Assign "b" (Union (Union (MakeSet (Variable "a")))) (Return "b"))) testKVS, Just [([],Set [9]),([],Set [9])]),
  (typeCheckInterpret (Assign "a" (Min (Sum DATA)) (Assign "b" (Intersection (Intersection (MakeSet (Variable "a")))) (Return "b"))) testKVS, Just [([],Set [9]),([],Set [9])]),
  (typeCheckInterpret (Assign "x" (Min (Product DATA)) (Assign "y" (Intersection (Intersection (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [-30]),([],Set [-30])]),
  (typeCheckInterpret (Assign "x" (Min (Product DATA)) (Assign "y" (Union (Union (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [-30]),([],Set [-30])]),
  (typeCheckInterpret (Assign "x" (Sum (Product DATA)) (Assign "y" (Intersection (Intersection (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [-30]),([],Set [-30])]),
  (typeCheckInterpret (Assign "x" (Sum (Product DATA)) (Assign "y" (Union (Union (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [-30]),([],Set [-30])]),
  (typeCheckInterpret (Assign "x" (Product (Product DATA)) (Assign "y" (Intersection (Intersection (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [-30]),([],Set [-30])]),
  (typeCheckInterpret (Assign "x" (Product (Product DATA)) (Assign "y" (Union (Union (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [-30]),([],Set [-30])]),
  (typeCheckInterpret (Assign "u" (Min (Min DATA)) (Assign "v" (Min (Min (Variable "u"))) (Return "v"))) testKVS, Just [([],Number (-1)),([],Number (-1))]),
  (typeCheckInterpret (Assign "u" (Product (Min DATA)) (Assign "v" (Product (Min (Variable "u"))) (Return "v"))) testKVS, Just [([],Number (-1)),([],Number (-1))]),
  (typeCheckInterpret (Assign "x" (Sum (Sum DATA)) (Assign "y" (Sum (Sum (Variable "x"))) (Return "y"))) testKVS, Just [([],Number 54),([],Number 54)]),
  (typeCheckInterpret (Assign "x" (Product (Min DATA)) (Assign "y" (Min (Min (Variable "x"))) (Return "y"))) testKVS, Just [([],Number (-1)),([],Number (-1))]),
  (typeCheckInterpret (Assign "x" (Product (Max DATA)) (Assign "y" (Max (Max (Variable "x"))) (Return "y"))) testKVS, Just [([],Number 5),([],Number 5)]),
  (typeCheckInterpret (Assign "x" (Product (Sum DATA)) (Assign "y" (Sum (Sum (Variable "x"))) (Return "y"))) testKVS, Just [([],Number 54),([],Number 54)])
  ]

testKVS = (dats [["T"], ["C", "D"], ["X", "Y", "Z"], ["a", "b"]] [-1,2,3,5]) !! 3

--eof