module TypeInf where

import Data.List

type VarTypeName = Int
data Type = VarType VarTypeName | FunType Type Type

instance (Show Type) where
  show (VarType x) = (show x)
  show (FunType (VarType x) (VarType y)) = (show x) ++ "->" ++ (show y)
  show (FunType (VarType x) t) = (show x) ++ "->(" ++ (show t) ++ ")"
  show (FunType t (VarType x)) = "(" ++ (show t) ++ ")->" ++ (show x)
  show (FunType t1 t2) = "(" ++ (show t1) ++ ")->(" ++ (show t2) ++ ")"

instance (Eq Type) where
  VarType x == VarType y           = x == y
  FunType t1 t2 == FunType u1 u2   = t1 == u1 && t2 == u2

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
inferType term =
  case inferType' term 1 of
    Just (result, _) -> Just result
    Nothing -> Nothing

inferType' :: LambdaTerm -> Int -> Maybe (TermTypeInfo, Int)

inferType' (Var x) newType =
  let
    freshContext = [(x, typeOfX)]
    typeOfX = VarType newType
  in Just ((freshContext, typeOfX), newType + 1)

inferType' (App m1 m2) newType
  | inferType' m1 0 == Nothing = Nothing
  | inferType' m2 0 == Nothing = Nothing
  | otherwise =
    let
      Just ((m1TypeContext, m1Type), newType') = inferType' m1 newType
      Just ((m2TypeContext, m2Type), newType'') = inferType' m2 newType'
      m1TypeContextDomain = fst $ unzip m1TypeContext
      m2TypeContextDomain = fst $ unzip m2TypeContext
      contextsDomainIntersect = Data.List.intersect m1TypeContextDomain m2TypeContextDomain
      maybeTheta1Unification = unifyContexts m1TypeContext m2TypeContext contextsDomainIntersect
    in
      case maybeTheta1Unification of
        Nothing -> Nothing
        Just theta1Unification ->
          let
            m1TypeTheta1 = applyUnificatorToType theta1Unification m1Type
            m2TypeTheta1 = applyUnificatorToType theta1Unification m2Type
            maybeTheta2Unification = unifyTypes m1TypeTheta1 (FunType m2TypeTheta1 (VarType newType''))
          in
            case maybeTheta2Unification of
              Nothing -> Nothing
              Just theta2Unification ->
                let
                  m1TypeContext' = applyUnificatorToTypeContext theta1Unification m1TypeContext
                  m2TypeContext' = applyUnificatorToTypeContext theta1Unification m2TypeContext
                  m1TypeContext'' = applyUnificatorToTypeContext theta2Unification m1TypeContext'
                  m2TypeContext'' = applyUnificatorToTypeContext theta2Unification m2TypeContext'
                  m1m2TypeContextUnion = Data.List.nub (m1TypeContext'' ++ m2TypeContext'')
                  appTypeTheta2Applied = applyUnificatorToType theta2Unification (VarType newType'')
                in
                  Just ((m1m2TypeContextUnion, appTypeTheta2Applied), newType'' + 1)


inferType' (Abs x m) newType
  | inferType' m 0 == Nothing = Nothing
  | otherwise =
    let
      Just ((mTypeContext, mType), newType') = inferType' m newType
    in
      case lookup x mTypeContext of
        Nothing -> Just ((mTypeContext, (FunType (VarType newType') mType)), newType' + 1)
        Just xType -> Just (((deleteFromSet x mTypeContext), (FunType xType mType)), newType')

showType :: LambdaTerm -> String
showType term = case inferType term of
                 Nothing -> "term " ++ (show term) ++ " has no type!"
                 Just (context, termType) -> (showContext context) ++ (show term) ++ " : " ++ (show termType)

showContext :: TypeContext -> String
showContext [] = ""
showContext ((v, vt):vs) = v ++ " : " ++ (show vt) ++ "\n" ++ (showContext vs)

printType :: LambdaTerm -> IO ()
printType term = putStrLn $ showType term

type TypeUnificator = [(VarTypeName, Type)]

applyUnificatorToType :: TypeUnificator -> Type -> Type
applyUnificatorToType unif (VarType x) =
  case lookup x unif of
    Nothing -> (VarType x)
    Just typeAssignment -> typeAssignment
applyUnificatorToType unif (FunType t1 t2) =
  FunType (applyUnificatorToType unif t1) (applyUnificatorToType unif t2)

unifyTypes :: Type -> Type -> Maybe TypeUnificator
unifyTypes (VarType x) (VarType y) | x == y = Just []
unifyTypes (VarType x) t = Just [(x, t)]
unifyTypes (FunType t _) (FunType u _) | unifyTypes t u == Nothing = Nothing
unifyTypes (FunType _ t) (FunType _ u) | unifyTypes t u == Nothing = Nothing
unifyTypes (FunType t1 t2) (FunType u1 u2) =
  let
    Just unif1 = unifyTypes t1 u1
    Just unif2 = unifyTypes t2 u2
  in
    joinUnificators unif1 unif2
unifyTypes _ _ = Nothing

joinUnificators :: TypeUnificator -> TypeUnificator -> Maybe TypeUnificator
joinUnificators unif1 unif2 = joinUnificators' unif1 unif2 []

joinUnificators' :: TypeUnificator -> TypeUnificator -> TypeUnificator -> Maybe TypeUnificator
joinUnificators' [] unif acc = Just (unif ++ acc)
joinUnificators' ((x,t):xs) unif acc
  | lookup x unif == Nothing = joinUnificators' xs unif ((x,t):(acc))
  | lookup x unif == Just t = joinUnificators' xs (deleteFromSet x unif) ((x,t):(acc))
  | otherwise = Nothing

deleteFromSet :: Eq a => a -> [(a,b)] -> [(a,b)]
deleteFromSet elem list = deleteFromSet' elem list []

deleteFromSet' :: Eq a => a -> [(a,b)] -> [(a,b)] -> [(a,b)]
deleteFromSet' _ [] acc = acc
deleteFromSet' key ((k,_):es) acc | key == k = es ++ acc
deleteFromSet' key (e:es) acc = deleteFromSet' key es (e:acc)

unifyContexts :: TypeContext -> TypeContext -> [VarName] -> Maybe TypeUnificator
unifyContexts con1 con2 commonDomain = unifyContexts' con1 con2 commonDomain []

unifyContexts' :: TypeContext -> TypeContext -> [VarName] -> TypeUnificator -> Maybe TypeUnificator
unifyContexts' con1 con2 [] acc = Just acc
unifyContexts' con1 con2 (x:xs) acc =
  let
    Just type1 = lookup x con1
    Just type2 = lookup x con2
  in
    case unifyTypes type1 type2 of
      Nothing -> Nothing
      Just assign -> unifyContexts' (con1' assign) (con2' assign) xs (assign ++ acc)
    where
      con1' a = applyUnificatorToTypeContext a con1
      con2' a = applyUnificatorToTypeContext a con2

applyUnificatorToTypeContext :: TypeUnificator -> TypeContext -> TypeContext
applyUnificatorToTypeContext _ [] = []
applyUnificatorToTypeContext unif ((x,t):xs) =
  (x, newType):(applyUnificatorToTypeContext unif xs)
  where
    newType = applyUnificatorToType unif t


