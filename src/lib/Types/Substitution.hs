module Types.Substitution where

import qualified Data.Map as M
import Data.List (foldl1')
import Control.Monad (join, liftM2)

import Types.Core

type TypeDeps = M.Map Type Type
type Subst = M.Map Type Type

isomorphic :: Type -> Type -> Bool
isomorphic t1@(Prim _) t2@(Prim _) = t1 == t2
isomorphic (TypeVar _) _ = True
isomorphic _ (TypeVar _) = True
isomorphic (TypeCons c ts) (TypeCons c' ts') =
    if (c /= c') || (length ts /= length ts')
        then False
    else and $ zipWith isomorphic ts ts'

walk :: Type -> TypeDeps -> Type
walk t graph = case M.lookup t graph of
                    Just t' -> walk t' graph 
                    Nothing -> t

insert :: Type -> Type -> TypeDeps -> Either TypeError TypeDeps
insert t1@(TypeVar _) t2@(TypeVar _) graph = 
    if M.notMember t1 graph
        then return $ M.insert t1 t2 graph
    else if M.notMember t2 graph
        then return $ M.insert t2 t1 graph
    else if (walk t1 graph) == (walk t2 graph) 
        then return graph 
    else insert t1 (walk t2 graph) graph
        
insert t1@(TypeCons _ ts1) t2@(TypeCons _ ts2) graph =
    if isomorphic t1 t2
        then mergeDeps graph $ zip ts1 ts2
    else Left $ mismatch t1 t2

insert t1@(TypeVar _) t2 graph = do
    case M.lookup t1 graph of
        Nothing -> return $ M.insert t1 t2 graph
        _ -> let end = walk t1 graph in case (end, t2) of
                                            (TypeVar _, _) -> return $ M.insert end t2 graph
                                            (TypeCons _ ts1, TypeCons _ ts2) -> insert end t2 graph
                                            _ -> if end == t2 then return graph else Left $ mismatch end t2

insert t1 t2@(TypeVar _) graph = insert t2 t1 graph 

insert t1 t2 graph = if t1 == t2 then return graph else Left $ mismatch t1 t2

mergeDeps :: TypeDeps -> [(Type, Type)] -> Either TypeError TypeDeps
mergeDeps = foldr ((=<<) . uncurry insert) . pure

subst :: Subst -> Type -> Type
subst s t = case t of
                Prim _ -> t
                TypeVar _ -> case M.lookup t s of
                                Just t' -> t'
                                Nothing -> t
                TypeCons c ts -> TypeCons c $ map (subst s) ts

-- walk a deps -> a?

flatten :: TypeDeps -> Subst
flatten deps = M.foldrWithKey go M.empty deps
                where   go :: Type -> Type -> TypeDeps -> TypeDeps
                        go k a acc = case a of
                                        TypeVar _ ->    if M.notMember a deps
                                                            then M.union acc (M.fromList $ [(k, walk k deps), (a, a)]) 
                                                        else M.insert k (walk k deps) acc
                                        _ -> M.insert k (walk k deps) acc

unflatten :: Subst -> [(Type, Type)] 
unflatten s = let noLoops = M.filterWithKey (/=) s in M.toList noLoops 

substComp :: [Subst] -> Either TypeError Subst
substComp xs =  let build = (mergeDeps M.empty . unflatten) <$> xs 
                    in do
                        merged <- foldl1' go build
                        return $ flatten $ merged
                        where   go :: Either TypeError TypeDeps -> Either TypeError TypeDeps -> Either TypeError TypeDeps
                                go curr new = do
                                    curr' <- curr
                                    new' <- M.toList <$> new
                                    mergeDeps curr' new'   

union :: Type -> Type -> Either TypeError Subst
union t1@(TypeVar _) t2 = pure . M.fromList $ [(t1, t2)]
union t1 t2@(TypeVar _) = union t2 t1
union t1@(TypeCons _ ts1) t2@(TypeCons _ ts2) = if isomorphic t1 t2
                                                    then (>>= substComp) . sequence $ zipWith union ts1 ts2
                                                else Left $ mismatch t1 t2
union t1 t2 = if t1 == t2 then pure M.empty else Left $ mismatch t1 t2

-- Testing

t1 :: Type
t1 = TypeCons "->" [TypeVar "a", Prim "Int"]

t2 :: Type
t2 = TypeCons "->" [TypeVar "b", TypeVar "a"]

    -- testing mergeDeps
ts1 :: [Type]
ts1 = [TypeVar "d", TypeVar "a", TypeVar "c", TypeVar "a", TypeVar "d", Prim "Bool"]

ts2 :: [Type]
ts2 = [TypeVar "b", TypeVar "b", TypeVar "d", Prim "Int", TypeVar "a", Prim "Bool"]

ts3 :: [Type]
ts3 = [TypeVar "a", Prim "Int"]

ts4 :: [Type]
ts4 = [TypeVar "b", TypeVar "a"]

ts5 :: [Type]
ts5 = [TypeVar "a", TypeVar "b", TypeVar "b", TypeVar "d", Prim "Char", Prim "Bool", TypeVar "d", TypeVar "a", TypeCons "->" [TypeCons "->" [TypeVar "a", TypeVar "y"], TypeVar "c"], TypeCons "->" [TypeCons "->" [TypeVar "f", TypeVar "g"], TypeVar "h"]]
ts6 :: [Type]
ts6 = [TypeVar "b", TypeVar "c", TypeVar "d", TypeVar "b", Prim "Char", Prim "Bool", TypeVar "e", Prim "Bool", TypeCons "->" [TypeCons "->" [TypeVar "y",  Prim "Bool"], Prim "Bool"], TypeCons "->" [TypeCons "->" [TypeVar "g", TypeVar "h"], Prim "Int"]]
    -- testing mergeDeps for catching errors

ts7 :: [Type] 
ts7 = [TypeVar "a", TypeVar "b", TypeVar "a"]

ts8 :: [Type]
ts8 = [TypeVar "b", Prim "Int", Prim "Char"]

ts9 :: [Type]
ts9 = [TypeCons "->" [TypeVar "a", TypeVar "b"], TypeVar "a"]

ts10 :: [Type]
ts10 = [TypeCons "->" [TypeVar "c"], TypeVar "a"]

ts11 :: [Type]
ts11 = [TypeVar "d", TypeCons "->" [TypeVar "b", TypeVar "c"], Prim "Char"]

ts12 :: [Type]
ts12 = [TypeVar "a", TypeVar "a", TypeVar "d"]

ts13 :: [Type]
ts13 = [TypeVar "e", TypeVar "a", TypeVar "d"]

    -- testing subst 

t3 :: Type
t3 = TypeCons "->" [TypeCons "->" [TypeCons "->" [TypeVar "b",  Prim "Int"], TypeVar "a"], TypeVar "b"]

t4 :: Type 
t4 = TypeCons "->" [TypeCons "->" [TypeCons "->" [TypeVar "b",   TypeVar "a"], TypeVar "c"], Prim "Bool"]

t5 :: Type
t5 = TypeCons "->" [Prim "Int", Prim "Bool"]

s1 :: Subst
s1 = M.fromList $ [(TypeVar "b", Prim "Char"), (TypeVar "a", TypeVar "c")]

s2 :: Subst
s2 = M.fromList $ [(TypeVar "b", Prim "Int"), (TypeVar "a", Prim "Int"), (TypeVar "c", TypeVar "b")]    

s3 :: Subst
s3 = M.fromList $ [(TypeVar "a", Prim "Char")]
    --

    -- testing flatten

outFlatten :: Either TypeError Subst
outFlatten = flatten <$> mergeDeps (M.empty) (zip ts5 ts6)

    -- testing substComp

s4 :: Subst
s4 = M.fromList $ [(TypeVar "a", TypeVar "b")] 

s5 :: Subst
s5 = M.fromList $ [(TypeVar "b", Prim "Int")]

s6 :: Subst
s6 = M.fromList [(TypeVar "b", TypeVar "a")]

    --

s7 :: Subst
s7 = M.fromList $ [(TypeVar "a", TypeVar "b"), (TypeVar "c", TypeVar "d"), (TypeVar "e", TypeVar "f")]

s8 :: Subst
s8 = M.fromList $ [(TypeVar "d", Prim "Bool"), (TypeVar "b", TypeVar "c")]

s9 :: Subst
s9 = M.fromList $ [(TypeVar "e", Prim "Int")]

    -- testing union

t6 :: Type
t6 = TypeCons "->" ts5

t7 :: Type
t7 = TypeCons "->" ts6
