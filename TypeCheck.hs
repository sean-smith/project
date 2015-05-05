module TypeCheck where

import AbstractSyntax
import Parse

isin :: String -> [(String, Type)] -> Bool
isin x (v:vs) = if x == fst v then True else isin x vs
isin x [] = False

justValue :: Maybe a -> a
justValue (Just a) = a

isJust         :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

class Typeable a where
  typeCheck :: [(String, Type)] -> a -> Maybe Type

instance Typeable Exp where 
  typeCheck env (Max a) = 
    let r = typeCheck env a
    in if r /= Nothing && (justValue r) == TyNumber then 
      Just( TyNumber ) else Nothing
  typeCheck env DATA = Just(TyNumber)
  typeCheck env (Min a) = 
    let r = typeCheck env a
    in if r /= Nothing && (justValue r) == TyNumber then 
      Just( TyNumber ) else Nothing
  typeCheck env (Sum a) = 
    let r = typeCheck env a
    in if r /= Nothing && (justValue r) == TyNumber then 
      Just( TyNumber ) else Nothing
  typeCheck env (Product a) = 
    let r = typeCheck env a
    in if r /= Nothing && (justValue r) == TyNumber then 
      Just( TyNumber ) else Nothing
  typeCheck env (Union a) = 
    let r = typeCheck env a
    in if r /= Nothing && (justValue r) == TySet then 
      Just( TySet ) else Nothing
  typeCheck env (Intersection a) = 
    let r = typeCheck env a
    in if r /= Nothing && (justValue r) == TySet then 
      Just( TySet ) else Nothing
  typeCheck env (MakeSet e) = 
    let r = typeCheck env e
    in if r /= Nothing && (justValue r) == TyNumber then 
      Just( TySet ) else Nothing
  typeCheck env (Variable x) =
    let l = filter (\v -> (fst v) == x) env 
    in if length l == 0 then Nothing else
      Just(snd (l!!0)) 


instance Typeable Stmt where 
  typeCheck env (Return x) =
    if x `isin` env then Just(TyVoid) else Nothing
  typeCheck env (Assign x e s) =
    let t1 = typeCheck env e
    in if t1 == Nothing then Nothing else
      let t2 = typeCheck (env ++ [(x, (justValue t1))]) s
      in t2



liftMaybe :: (a -> b) -> (Maybe a -> Maybe b)
-- Complete for Problem 2, part (c).
liftMaybe f (Just x) = Just (f x)
liftMaybe f Nothing = Nothing


joinMaybe :: Maybe (Maybe a) -> Maybe a
-- Complete for Problem 2, part (c).
joinMaybe (Just (Just x)) = Just x
joinMaybe _  = Nothing

tokenizeParseTypeCheck :: String -> Maybe Type
-- Complete for Problem 2, part (d).
-- tokenizeParse :: String -> Maybe a
-- typeCheck :: [(String, Type)] -> a -> Maybe Type


tokenizeParseTypeCheck s = joinMaybe( liftMaybe ((typeCheck:: [(String, Type)] -> Stmt -> Maybe Type) []) (tokenizeParse s))
  
-- eof