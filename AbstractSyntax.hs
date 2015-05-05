{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module AbstractSyntax where

import Data.List (union, intersect)

data Exp =
    DATA
  | Variable String
  | Max Exp
  | Min Exp
  | Sum Exp
  | Product Exp
  | Union Exp
  | Intersection Exp
  | MakeSet Exp
  deriving (Eq, Show)

data Stmt =
    Assign String Exp Stmt
  | Return String
  deriving (Eq, Show)

data Type =
    TyNumber
  | TySet
  | TyVoid
  deriving (Eq, Show)

data Value =
    Set [Integer]
  | Number Integer
  | Error
  deriving (Eq, Show)

instance Num Value where
  fromInteger n = Number n
  Number a + Number b = Number(a + b)
  Number a * Number b = Number(a * b)
  
instance Ord Value where
  Number a < Number b = a < b
  Number a <= Number b = a <= b



(\/) :: Value -> Value -> Value
(\/) (Set a) (Set b) = Set (a `union` b)
(\/) _ _ = Error

(/\) :: Value -> Value -> Value
(/\) (Set a) (Set b) = Set (a `intersect` b)
(/\) _ _ = Error

-- Type class Foldable for a fold function on data types.
--
--  b* The first argument is a constant that will replace all
--    leaf nodes that contain no variable.
--  v* The second argument is a function that will be applied to
--    (i.e., and will replace) any variables.
--  f* The third argument is the aggregator for combining
--    results of recursive folds.
--  data* The fourth argument is the data value that will be folded.

class Foldable a where
  fold :: b -> (String -> b) -> ([b] -> b) -> a -> b

instance Foldable Exp where
  fold b v f (Variable e) = v e
  fold b v f DATA = b
  fold b v f (Max e) = f [fold b v f e]
  fold b v f (Min e) = f [fold b v f e]
  fold b v f (Sum e) = f [fold b v f e]
  fold b v f (Product e) = f [fold b v f e]
  fold b v f (Union e) = f [fold b v f e]
  fold b v f (Intersection e) = f [fold b v f e]
  fold b v f (MakeSet e) = f [fold b v f e]

instance Foldable Stmt where
  fold b v f (Return a) = b
  --          Assign String Exp Stmt
  fold b v f (Assign a d c) = f [fold b v f d, fold b v f c]
  -- I'm not sure about the comma in the above maybe []++[]

--In AbstractSyntax.hs, define vars :: Stmt -> [String] so 
--that it returns a list containing every variable found in the abstract syntax tree given to it
vars :: Stmt -> [String]
vars s = fold [] (\x -> [x]) concat s


--Also define operations :: Stmt -> Integer, which returns the exact number of operations
-- found in the abstract syntax tree given to it.
operations :: Stmt -> Integer
--                  b     v          f        e
operations s = fold 0 (\x -> 0) (\x -> sum ([1]++x)) s

-- eof