module TypeInf.TypeDefs where

import Prelude
import Data.Char

-- type variables for now are just integers
type VarTypeName = Int
-- types are variable types or function types
data Type = VarType VarTypeName | FunType Type Type deriving Eq

-- instance of show to pretty print types
instance Show Type where
  show (VarType x) = [chr (944 + x)]
  show (FunType (VarType x) t) = show (VarType x) ++ "->" ++ show t ++ ""
  show (FunType t (VarType x)) = "(" ++ show t ++ ")->" ++ show (VarType x)
  show (FunType t1 t2) = "(" ++ show t1 ++ ")->" ++ show t2

-- lambda term variable names
type VarName = String
-- lambda terms are variables, applications or lambda abstractions
data LambdaTerm = Var VarName |
                  App LambdaTerm LambdaTerm |
                  Abs [VarName] LambdaTerm
--                        deriving Show
-- instance of show to pretty print lambda terms
instance Show LambdaTerm where
  show (Var m) = m
  show (App (Abs xs m) (Abs ys n)) = "(" ++ show (Abs xs m) ++ ") (" ++ show (Abs ys n) ++ ")"
  show (App (Abs xs m) n) = "(" ++ show (Abs xs m) ++ ") " ++ show n
  show (App m (Var x)) = show m ++ " " ++ x
  show (App m1 m2) = show m1 ++ " (" ++ show m2 ++ ")"
  show (Abs xs m) = "\955" ++ foldl1 (\a b -> a ++ " " ++ b) xs ++ "." ++ show m

-- type context is mapping from variable name to type
type TypeContext = [(VarName, Type)]
-- lambda term type info is type context + lambda term type
type TermTypeInfo = (TypeContext, Type)

