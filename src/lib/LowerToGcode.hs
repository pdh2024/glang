{-# LANGUAGE TupleSections #-}
module LowerToGcode where

import Control.Monad.State
import Data.List (partition)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Set as S

import Expression
import Parser
import LambdaLifting

-- Inst

data Inst =
        Push Int
    |   PushSC VarName
    |   PushLit Expr
    |   Update Int
    |   Pop Int
    |   PerformOp OpName
    |   MakeApp
    |   Unwind
    |   Rewind
    deriving (Show, Eq)

-- Lowering

compile' :: M.Map VarName Int -> Expr -> [Inst]
compile' _ lit@(Literal _ _) = [PushLit lit]
compile' env (Op o) = case o of
                        "cond" -> normalizeArg 0 ++ [PerformOp o]
                        _ -> concatMap normalizeArg [0..(length env - 1)] ++ [PerformOp o] 
compile' env (Var v) =  if M.member v env
                            then [Push $ fromJust $ M.lookup v env]
                        else [PushSC v]
compile' env (App e1 e2) = compile' env e2 ++ compile' env' e1 ++ [MakeApp]
    where   env' = M.map (+1) env
    
normalizeArg :: Int -> [Inst]
normalizeArg i = [Push i, Unwind, Update i]
    
compile :: SCDef -> [Inst]
compile (SCDef vs body) =   let compiledBody = compile' (M.fromList $ zip vs [0..]) body
                                arity = length vs
                                in  if null vs
                                        then compiledBody ++ [Unwind]
                                    else compiledBody ++ [Update arity, Pop arity, Unwind]
                                    
lower :: [(VarName, Expr)] -> M.Map VarName SCDef -> M.Map VarName (SCDef, [Inst])                                    
lower defs opsMap =  let    idents = S.fromList (map fst defs) `S.union` M.keysSet opsMap

                            liftedDefs' = mapM (\(v,expr) -> (v,) <$> lambdaLift' expr) defs
                            liftedDefs = evalState liftedDefs' (idents, 0)
                        
                            scDefs = foldr step opsMap liftedDefs
                            scInst = M.map (\scdef -> (scdef, compile scdef)) scDefs

                            in scInst
    where   step (v, (expr, internalMap)) map = M.insert v (caf expr) (M.union internalMap map)
