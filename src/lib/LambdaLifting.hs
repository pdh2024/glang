module LambdaLifting where

import Control.Monad.State
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Bifunctor (first)

import Expression

freeVars :: Expr -> S.Set VarName -> S.Set VarName
freeVars (Abst param body) scIdents = go (S.singleton param) S.empty body
    where   go _ free (Literal _ _) = free
            go bound free (Var v) =
                if S.member v bound || S.member v scIdents
                    then free
                else S.insert v free
            go bound free (App e1 e2) = S.union (go bound free e1) (go bound free e2)
            go bound free (Op _) = free
            go bound free (Abst _ _) = free
            
caf :: Expr -> SCDef
caf = SCDef [] 

lambdaLift' :: Expr -> State (S.Set VarName, Int) (Expr, M.Map VarName SCDef)
lambdaLift' (Abst param body) = do
    (scIdents, i) <- get
    let scName = "!sc_" ++ show i
    modify ((+1) <$>)
    
    -- If an identifier is bound by an abstraction, it should be removed from the set of top-level identifiers
    -- Otherwise nested abstractions will treat it as "bound" via a top-level definition and compile to pushSC instructions rather than properly lifting it
    modify (first (S.delete param))

    (scBody, scDefs) <- lambdaLift' body
    let free = S.toList $ freeVars (Abst param scBody) (M.keysSet scDefs `S.union` scIdents)

    let sc = SCDef (free ++ [param]) scBody
    let expr' = if null free
                    then Var scName
                else foldl App (Var scName) (Var <$> free)

    return (expr', M.insert scName sc scDefs)

lambdaLift' (App e1 e2) = do
    (e1', e1scDefs) <- lambdaLift' e1
    (e2', e2scDefs) <- lambdaLift' e2
    return (App e1' e2', M.union e1scDefs e2scDefs)

lambdaLift' expr = return (expr, M.empty)

lambdaLift :: Expr -> S.Set VarName -> (SCDef, M.Map VarName SCDef)
lambdaLift expr scIdents = (sc, scDefs)
    where   (expr', scDefs) = evalState (lambdaLift' expr) (scIdents, 0)
            sc = caf expr' 
            