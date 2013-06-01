module TypeInf where

import Data.List
import TypeDefs
import LambdaParser
import Unification


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
            maybeTheta2Unification = unify [(m1TypeTheta1, (FunType m2TypeTheta1 (VarType newType'')))]
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

-- rename types to use adjacent names (starting from 1)
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

-- applies type assignment to specified type
assignTypeNames :: [(VarTypeName, VarTypeName)] -> Type -> Type
assignTypeNames assignment (VarType x) = case lookup x assignment of
  Just y -> VarType y
  Nothing -> VarType x
assignTypeNames assignment (FunType t1 t2) = FunType t1' t2'
  where
    t1' = assignTypeNames assignment t1
    t2' = assignTypeNames assignment t2

-- gather type names existing in type
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

-- parse input and print type
typ :: String -> IO ()
typ input =
  let term = parseLambdaTerm input
  in printType term

