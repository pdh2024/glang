module Types.TypeInference where

import Control.Monad.Trans.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad (forM)
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Expression
import Types.Core
import Types.Substitution

type Context = M.Map VarName Scheme

type Inference = ExceptT TypeError (State (Int, Context)) (Subst, Type)

makeVar :: Int -> VarName
makeVar i = "__v" ++ show i

getVars :: Type -> [VarName]
getVars (TypeVar v) = [v]
getVars (Prim _) = []
getVars (TypeCons _ ts) = concat $ getVars <$> ts

substContext :: Subst -> Context -> Context
substContext s c = M.map (\(Scheme vs t) -> Scheme vs $ subst s t) c

instantiate :: Int -> Scheme -> (Type, Int)
instantiate i (Scheme tvs t) = let s = M.fromList $ [(TypeVar old, TypeVar $ makeVar $ i+k) | (old, k) <- zip tvs [1..]]
                                in (subst s t, i + length tvs)

generalize :: Context -> Scheme -> Scheme
generalize context (Scheme tvs t) = let bound = M.foldr (\(Scheme tvs _ ) bound -> tvs ++ bound) [] context
                                        tvs' = filter (\v -> (notElem v tvs) && (notElem v bound)) tvs
                                        in Scheme (tvs ++ tvs') t

inferExpr :: Expr -> Inference
inferExpr (Literal t v) = return (M.empty, Prim t)

inferExpr (Var v) = do
    (i, context) <- get
    case M.lookup v context of
        Just scheme -> do
            (t', i') <- return $ instantiate i scheme
            put (i', context)
            return (M.empty, t')
        Nothing -> throwE $ TypeError ("Identifier " ++ show v ++ " cannot be found.")

inferExpr (Abst param expr) = do
    (i, context) <- get
    paramVar <- return $ makeVar (i+1)
    let context' = M.insert param (Scheme [] $ TypeVar paramVar) (M.delete param context)
        in put (i+1, context')
    (s, t) <- inferExpr expr
    put (i+1, context)
    return (s, TypeCons "->" [subst s (TypeVar paramVar), t])

inferExpr (App e1 e2) = do
    (s1, t1) <- inferExpr e1
    (i, context) <- get
    outVar <- return $ makeVar $ i+1
    put (i+1, substContext s1 context)
    (s2, t2) <- inferExpr e2
    case union (subst s2 t1) (TypeCons "->" [t2, TypeVar outVar]) of
        Right v ->  case (substComp [s1, s2, v]) of
                        Right scomp -> return (scomp, subst v $ TypeVar outVar)
                        Left e -> throwE e    
        Left e -> throwE e

inferExpr (Let x e1 e2) = do
    (s1, t1) <- inferExpr e1
    (i, context) <- get
    let context' = substContext s1 context
        context'' = M.insert x (generalize context' (Scheme [] t1)) (M.delete x context')
        in put (i, context'')
    (s2, t2) <- inferExpr e2
    case substComp [s1, s2] of
        Right scomp -> return (scomp, t2)
        Left e -> throwE e

-- testing

runInference :: Expr -> Context -> Either TypeError (Subst, Type)
runInference expr context = let inf = inferExpr expr in ((evalState . runExceptT $ inf) (0,context))

identContext :: Context
identContext =  M.fromList [("id", Scheme ["a"] (TypeCons "->" [TypeVar "a", TypeVar "a"]))]

identOut :: Either TypeError (Subst, Type)
identOut = runInference (Var "id") identContext

absOut :: Either TypeError (Subst, Type)
absOut = runInference (Abst "x" (Var "x")) (M.empty)

absError :: Either TypeError (Subst, Type)
absError = runInference (Abst "x" (Var "y")) (M.empty)

appOut :: Either TypeError (Subst, Type)
appOut = runInference (App (Abst "x" (Var "x")) (Literal "Int" "3")) (M.empty)