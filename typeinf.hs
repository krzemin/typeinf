module TypeInf where

import Data.List
import Data.Char

-- type variables for now are just integers
type VarTypeName = Int
-- types are variable types or function types
data Type = VarType VarTypeName | FunType Type Type

-- instance of show to pretty print types
instance (Show Type) where
  show (VarType x) = [chr (944 + x)]
  show (FunType (VarType x) (VarType y)) = (show (VarType x)) ++ "->" ++ (show (VarType y))
  show (FunType (VarType x) t) = (show (VarType x)) ++ "->(" ++ (show t) ++ ")"
  show (FunType t (VarType x)) = "(" ++ (show t) ++ ")->" ++ (show (VarType x))
  show (FunType t1 t2) = "(" ++ (show t1) ++ ")->" ++ (show t2)

-- comparing types for equality 
instance (Eq Type) where
  VarType x == VarType y           = x == y
  FunType t1 t2 == FunType u1 u2   = t1 == u1 && t2 == u2

-- lambda term variable names
type VarName = String
-- lambda terms are variables, applications or lambda abstractions
data LambdaTerm = Var VarName |
                  App LambdaTerm LambdaTerm |
                  Abs VarName LambdaTerm |
                  MultiAbs [VarName] LambdaTerm

-- instance of show to pretty print lambda terms
instance (Show LambdaTerm) where
  show (Var m) = m
  show (App (Var x) (Var y)) = x ++ " " ++ y
  show (App (Var x) m) = x ++ " (" ++ (show m) ++ ")"
  show (App m (Var x)) = "(" ++ (show m) ++ ") " ++ x
  show (App m1 m2) = "(" ++ (show m1) ++ ") (" ++ (show m2) ++ ")"
  show (Abs x (Var y)) = "\955" ++ x ++ "." ++ y
  show (Abs x m) = "\955" ++ x ++ "." ++ (show m)
  show (MultiAbs [x] m) = show (Abs x m)
  show (MultiAbs xs m) = "\955" ++ (drop 1 $ foldl (\a b -> a ++ " " ++ b) "" xs) ++ "." ++ (show m) 

-- rebuild multi abstraction as normal abstraction tree
multiAbsToAbs :: LambdaTerm -> LambdaTerm
multiAbsToAbs (MultiAbs [x] m) = Abs x m
multiAbsToAbs (MultiAbs (x:xs) m) = Abs x (multiAbsToAbs (MultiAbs xs m))
multiAbsToAbs term = term

-- rebuild normal abstraction tree and replaces possible abstractions with multi abstractions
extractMultiAbs :: LambdaTerm -> LambdaTerm
extractMultiAbs (App m n) = App (extractMultiAbs m) (extractMultiAbs n)
extractMultiAbs (Abs x (Abs y m)) = case m of
  (Abs _ _) ->
    let MultiAbs ys m' = extractMultiAbs (Abs y m)
    in  MultiAbs (x:ys) m'
  otherwise -> MultiAbs [x,y] m
extractMultiAbs term = term

-- TODO: beta reduction rules

-- type context is mapping from variable name to type
type TypeContext = [(VarName, Type)]
-- lambda term type info is type context + lambda term type
type TermTypeInfo = (TypeContext, Type)

-- main function to infering types of lambda terms
inferType :: LambdaTerm -> Maybe TermTypeInfo
inferType term =
  case inferType' term 1 of
    Just (result, _) -> Just result
    Nothing -> Nothing

-- helper function to infering types of lambda terms (aware of fresh type variable generation)
inferType' :: LambdaTerm -> VarTypeName -> Maybe (TermTypeInfo, VarTypeName)

-- type infering of lambda variable
inferType' (Var x) newType =
  let
    freshContext = [(x, typeOfX)]
    typeOfX = VarType newType
  in Just ((freshContext, typeOfX), newType + 1)

-- type infering of lambda application
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

-- type infering of lambda abstraction
inferType' (Abs x m) newType
  | inferType' m 0 == Nothing = Nothing
  | otherwise =
    let
      Just ((mTypeContext, mType), newType') = inferType' m newType
    in
      case lookup x mTypeContext of
        Nothing -> Just ((mTypeContext, (FunType (VarType newType') mType)), newType' + 1)
        Just xType -> Just (((deleteFromSet x mTypeContext), (FunType xType mType)), newType')

-- type infering of multi abstraction is rebuilding it as normal abstraction first
inferType' (MultiAbs xs m) newType =
  let abs = multiAbsToAbs (MultiAbs xs m)
  in inferType' abs newType

reenumerateTermTypeInfo :: TermTypeInfo -> TermTypeInfo
reenumerateTermTypeInfo (context, termType) =
  let
    typeNamesFromCollection = collectTypeNames termType
    typesFromContext = snd $ unzip context
    allTypeNames = foldl collectTypeNames' typeNamesFromCollection typesFromContext
    typeNamesAssignment = zip allTypeNames [1..]
    context' = zip (fst $ unzip context) (map (assignTypeNames typeNamesAssignment) typesFromContext)
    termType' = assignTypeNames typeNamesAssignment termType
  in
    (context', termType')

assignTypeNames :: [(VarTypeName, VarTypeName)] -> Type -> Type
assignTypeNames assignment (VarType x) = case lookup x assignment of
  Just y -> VarType y
  Nothing -> VarType x
assignTypeNames assignment (FunType t1 t2) = FunType t1' t2'
  where
    t1' = assignTypeNames assignment t1
    t2' = assignTypeNames assignment t2

collectTypeNames :: Type -> [VarTypeName]
collectTypeNames = collectTypeNames' []

collectTypeNames' :: [VarTypeName] -> Type -> [VarTypeName]
collectTypeNames' acc (VarType x)
  | elem x acc = acc
  | otherwise = acc ++ [x]
collectTypeNames' acc (FunType t1 t2) =
  let acc' = collectTypeNames' acc t1
  in  collectTypeNames' acc' t2

-- makes string with context and type of lambda term
showType :: LambdaTerm -> String
showType term = case inferType term of
  Nothing -> "term " ++ (show term) ++ " has no type!"
  Just (context, termType) -> (showContext context') ++ (show term) ++ " : " ++ (show termType')
    where
      (context', termType') = reenumerateTermTypeInfo (context, termType)

-- make string out of type context
showContext :: TypeContext -> String
showContext [] = ""
showContext ((v, vt):vs) = v ++ " : " ++ (show vt) ++ "\n" ++ (showContext vs)

-- print lambda term (with context also) to stdout
printType :: LambdaTerm -> IO ()
printType term = putStrLn $ showType term

-- unificator is mapping from type variable to another type
type TypeUnificator = [(VarTypeName, Type)]

-- applying unificator to specified type 
applyUnificatorToType :: TypeUnificator -> Type -> Type
applyUnificatorToType unif (VarType x) =
  case lookup x unif of
    Nothing -> (VarType x)
    Just typeAssignment -> typeAssignment
applyUnificatorToType unif (FunType t1 t2) =
  FunType (applyUnificatorToType unif t1) (applyUnificatorToType unif t2)

-- unification of two types may be successfull or not
unifyTypes :: Type -> Type -> Maybe TypeUnificator
unifyTypes (VarType x) (VarType y) | x == y = Just []
unifyTypes (VarType x) t
  | nameOccursInType x t = Nothing 
  | otherwise = Just [(x, t)]
unifyTypes (FunType t _) (FunType u _) | unifyTypes t u == Nothing = Nothing
unifyTypes (FunType _ t) (FunType _ u) | unifyTypes t u == Nothing = Nothing
unifyTypes (FunType t1 t2) (FunType u1 u2) =
  let
    Just unif1 = unifyTypes t1 u1
    Just unif2 = unifyTypes t2 u2
  in
    joinUnificators unif1 unif2
unifyTypes _ _ = Nothing

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
  | lookup x unif == Nothing = joinUnificators' xs unif ((x,t):(acc))
  | lookup x unif == Just t = joinUnificators' xs (deleteFromSet x unif) ((x,t):(acc))
  | otherwise = Nothing

-- function removing element for set represented as list of (key,value) pairs
deleteFromSet :: Eq a => a -> [(a,b)] -> [(a,b)]
deleteFromSet elem list = deleteFromSet' elem list []

-- helper function for removing element from set (represented as above)
deleteFromSet' :: Eq a => a -> [(a,b)] -> [(a,b)] -> [(a,b)]
deleteFromSet' _ [] acc = acc
deleteFromSet' key ((k,_):es) acc | key == k = es ++ acc
deleteFromSet' key (e:es) acc = deleteFromSet' key es (e:acc)

-- unifying type contexts in some domain may be successfull or not
unifyContexts :: TypeContext -> TypeContext -> [VarName] -> Maybe TypeUnificator
unifyContexts con1 con2 commonDomain = unifyContexts' con1 con2 commonDomain []

-- helper function for unifying type contexts
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

-- applying unificator to type context produces new type context (with some types renamed)
applyUnificatorToTypeContext :: TypeUnificator -> TypeContext -> TypeContext
applyUnificatorToTypeContext _ [] = []
applyUnificatorToTypeContext unif ((x,t):xs) =
  (x, newType):(applyUnificatorToTypeContext unif xs)
  where
    newType = applyUnificatorToType unif t


