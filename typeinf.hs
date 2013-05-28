module TypeInf where

type VarTypeName = Int
data Type = VarType VarTypeName | FunType Type Type

instance (Show Type) where
  show (VarType x) = (show x)
  show (FunType (VarType x) (VarType y)) = (show x) ++ "->" ++ (show y)
  show (FunType (VarType x) t) = (show x) ++ "->(" ++ (show t) ++ ")"
  show (FunType t (VarType x)) = "(" ++ (show t) ++ ")->" ++ (show x)
  show (FunType t1 t2) = "(" ++ (show t1) ++ ")->(" ++ (show t2) ++ ")"

type VarName = String
data LambdaTerm = Var VarName | App LambdaTerm LambdaTerm | Abs VarName LambdaTerm

instance (Show LambdaTerm) where
  show (Var m) = m
  show (App (Var x) (Var y)) = x ++ y
  show (App (Var x) m) = x ++ "(" ++ (show m) ++ ")"
  show (App m (Var x)) = "(" ++ (show m) ++ ")" ++ x
  show (App m1 m2) = "(" ++ (show m1) ++ ")" ++ "(" ++ (show m2) ++ ")"
  show (Abs x (Var y)) = "\\" ++ x ++ "." ++ y
  show (Abs x m) = "\\" ++ x ++ ".(" ++ (show m) ++ ")"

type TypeContext = [(VarName, Type)]
type TermTypeInfo = (TypeContext, Type)

inferType :: LambdaTerm -> Maybe TermTypeInfo
inferType term = case inferType' term 1 of
                   Just (result, _) -> Just result
                   Nothing -> Nothing

inferType' :: LambdaTerm -> Int -> Maybe (TermTypeInfo, Int)

inferType' (Var x) newType =
  let
    freshContext = [(x, typeOfX)]
    typeOfX = VarType newType
  in Just ((freshContext, typeOfX), newType + 1)
{--
inferType' (App m1 m2) newType
  | inferType m1 newType == Nothing -> Nothing
  | inferType m2 newType == Nothing -> Nothing
  | otherwise ->
    let
      ((m1TypeContext, m1Type), newType') = inferType m1 newType
      ((m2TypeContext, m2Type), newType'') = inferType m2 newType'
      m1TypeContextDomain = fst $ unzip m1Context
      m2TypeContextDomain = fst $ unzip m2Context
      contextsDomainIntersect = Data.List.intersect m1TypeContextDomain m2TypeContextDomain
      maybeTheta1Unification = unify m1TypeContext m2TypeContext contextsDomainIntersect
    in
      case maybeTheta1Unification of
        Nothing -> Nothing
        Just theta1Unification ->
          let
            
   --}   

showType :: LambdaTerm -> String
showType term = case inferType term of
                 Nothing -> "term " ++ (show term) ++ " has no type!"
                 Just (context, termType) -> (show term) ++ " : " ++ (show termType)







