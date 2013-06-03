module TypeInf.Unification where

import Prelude
import Data.Maybe
import TypeInf.TypeDefs

-- unificator is mapping from type variable to another type
type TypeUnificator = [(VarTypeName, Type)]

-- applying unificator to type context produces new type context (with some types renamed)
applyUnificatorToTypeContext :: TypeUnificator -> TypeContext -> TypeContext
applyUnificatorToTypeContext _ [] = []
applyUnificatorToTypeContext unif ((x,t):xs) =
  (x, newType):applyUnificatorToTypeContext unif xs
  where
    newType = applyUnificatorToType unif t

-- applying unificator to specified type 
applyUnificatorToType :: TypeUnificator -> Type -> Type
applyUnificatorToType unif (VarType x) =
  fromMaybe (VarType x) $ lookup x unif
applyUnificatorToType unif (FunType t1 t2) =
  FunType (applyUnificatorToType unif t1) (applyUnificatorToType unif t2)

-- checking whether type name is used in specified type
nameOccursInType :: VarTypeName -> Type -> Bool
nameOccursInType x (VarType y) = x == y
nameOccursInType x (FunType t1 t2) = nameOccursInType x t1 || nameOccursInType x t2

-- joining two unificators may be successfull or not
joinUnificators :: TypeUnificator -> TypeUnificator -> Maybe TypeUnificator
joinUnificators unif1 unif2 = joinUnificators' unif1 unif2 []

-- helper function for joining two unificators
joinUnificators' :: TypeUnificator -> TypeUnificator -> TypeUnificator -> Maybe TypeUnificator
joinUnificators' [] unif acc = Just (unif ++ acc)
joinUnificators' ((x,t):xs) unif acc
  | isNothing $ lookup x unif = joinUnificators' xs unif ((x,t):acc)
  | lookup x unif == Just t = joinUnificators' xs (deleteFromSet x unif) ((x,t):acc)
  | otherwise = Nothing

deleteFromSet :: Eq a => a -> [(a, b)] -> [(a, b)]  
deleteFromSet keyToRemove = filter (\(key, _) -> key /= keyToRemove)

-- unifying type contexts in some domain may be successfull or not
unifyContexts :: TypeContext -> TypeContext -> [VarName] -> Maybe TypeUnificator
unifyContexts con1 con2 commonDomain = unify unifyInput
  where
    unifyInput = collectCommonDomain con1 con2 commonDomain []

collectCommonDomain :: TypeContext -> TypeContext -> [VarName] -> [(Type, Type)] -> [(Type, Type)]
collectCommonDomain _ _ [] acc = acc
collectCommonDomain con1 con2 (x:xs) acc = collectCommonDomain con1 con2 xs acc'
  where
    Just t1 = lookup x con1
    Just t2 = lookup x con2
    acc' = (t1, t2):acc

-- unification
unify :: [(Type, Type)] -> Maybe TypeUnificator
unify [] = Just []
unify ((VarType x, VarType y):ts) | x == y = unify ts
unify ((VarType x, t):ts)
  | nameOccursInType x t = Nothing
  | otherwise =
    case unify (replaceVarInTypes x t ts) of
      Nothing -> Nothing
      Just unificator -> Just ((x, t):unificator)
unify ((t, VarType x):ts) = unify ((VarType x, t):ts)
unify ((FunType t1 t2, FunType u1 u2):ts) = unify ((t1,u1):(t2,u2):ts)

-- replaces type variable occurrences in specified list of pairs of types
replaceVarInTypes :: VarTypeName -> Type -> [(Type, Type)] -> [(Type, Type)]
replaceVarInTypes x t = map replaceVarInTypes'
  where
    replaceVarInTypes' (t1, t2) = (replaceVarInType x t t1, replaceVarInType x t t2)

-- replaces type variable in specified type with given type
replaceVarInType :: VarTypeName -> Type -> Type -> Type
replaceVarInType x t (VarType y)
  | x == y = t
  | otherwise = VarType y
replaceVarInType x t (FunType t1 t2) = FunType t1' t2'
  where
    t1' = replaceVarInType x t t1
    t2' = replaceVarInType x t t2


