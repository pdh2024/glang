{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module GraphReduction where

import Data.Functor ((<&>))
import Control.Monad.ST
import qualified Data.Vector.Mutable as V
import Data.Array.ST
import Data.Array.Base (getNumElements)
import Control.Monad (liftM2)
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Expression
import Debug.Trace (trace)

data Node = 
        LitNode TypeName Value
    |   VarNode VarName
    |   AbstNode
    |   OpNode OpName
    |   AppNode
    |   LetNode VarName
    |   Nil
    deriving (Show, Eq)

appBranches :: Expr -> Int
appBranches (Let _ _ e2) = appBranches e2
appBranches (App e1 e2) = 1 + max (appBranches e1) (appBranches e2)
appBranches expr = 0

size :: Expr -> Int
size expr = 1 + go (appBranches expr)
            where   go 0 = 0
                    go 1 = 2
                    go n = 2^n + go (n-1)

type Reduction a = forall s. StateT (V.STVector s Node, M.Map VarName (Int, Bool)) (ST s) a

write :: Int -> Node -> Reduction ()
write idx node = do
    vec <- gets fst
    map <- gets snd
    let length = V.length vec
    vec' <- if idx >= length
                then lift $ V.grow vec length
            else pure vec
    put (vec', map)
    lift $ V.write vec' idx node

populate :: Int -> Expr -> Reduction ()
populate idx (Literal t v) = write idx (LitNode t v)
populate idx (Var v) = write idx (VarNode v)
populate idx (Op o) = write idx (OpNode o) 
populate idx expr = do
    case expr of
        Abst param body -> do
            write idx AbstNode 
            populate (l idx) (Var param) 
            populate (r idx) body 
        App e1 e2 -> do
            write idx AppNode 
            populate (l idx) e1 
            populate (r idx) e2
        Let v e1 e2 -> do
            modify (M.insert v (l idx, False) <$>)
            write idx (LetNode v) 
            populate (l idx) e1 
            populate (r idx) e2

graphToExpr :: Int -> Reduction Expr
graphToExpr idx = do
    vec <- gets fst
    node <- lift $ V.read vec idx
    case node of
        LitNode t v -> return $ Literal t v
        VarNode v -> return $ Var v
        AbstNode -> do
            (Var param) <- graphToExpr (l idx) 
            graphToExpr (r idx) <&> Abst param
        OpNode o -> return $ Op o
        AppNode -> liftM2 App (graphToExpr (l idx)) (graphToExpr (r idx))
        LetNode _ -> graphToExpr (r idx) 

reduce' :: Expr -> Reduction Expr
reduce' expr = do
    populate 0 expr 
    normalize 0 
    graphToExpr 0 

reduce :: Expr -> Expr
reduce expr = runST $ do
    graph <- V.replicate (size expr) Nil
    evalStateT (reduce' expr) (graph, M.empty)

writeFrom :: Int -> Int -> Reduction ()
writeFrom fromIdx toIdx = do
    vec <- gets fst
    node <- lift $ V.read vec fromIdx
    case node of
        AbstNode -> do
            write toIdx node 
            writeFrom (l fromIdx) (l toIdx) 
            writeFrom (r fromIdx) (r toIdx) 
        AppNode -> do
            write toIdx node 
            writeFrom (l fromIdx) (l toIdx) 
            writeFrom (r fromIdx) (r toIdx) 
        LetNode _ -> do
            writeFrom (r fromIdx) (r toIdx) 
        _ -> write toIdx node 


unwind :: Int -> V.STVector s Node -> StateT ([Int], [Int]) (ST s) (Int, [Int], [Int])
unwind idx vec = do
    node <- lift $ V.read vec idx
    case node of
        AppNode -> do
            noder <- lift $ V.read vec (r idx)
            (stack, preds) <- get
            put (r idx : stack, idx : preds)
            unwind (l idx) vec
        LetNode _ -> do
            noder <- lift $ V.read vec (r idx)
            unwind (r idx) vec
        _ -> do
            (stack, preds) <- get
            return (idx, stack, preds)

rewind :: Int -> Int -> Int
rewind idx 0 = idx
rewind idx count = rewind (div (idx - 1) 2) (count - 1) 

apply :: Int -> [Int] -> [Int] -> Reduction Bool
apply outerIdx [] _ = do
    vec <- gets fst
    outer <- lift $ V.read vec outerIdx
    case outer of
        VarNode v -> do
            vidx <- gets (fst . fromJust . M.lookup v . snd)
            writeFrom vidx outerIdx 
            return True
        _ -> return True

apply outerIdx stack preds = do
    vec <- gets fst
    outer <- lift $ V.read vec outerIdx
    case outer of
        VarNode v -> do
            vidx <- gets (fst . fromJust . M.lookup v . snd)
            writeFrom vidx outerIdx 
            apply outerIdx stack preds
        OpNode o -> do
            let arity = fromJust $ M.lookup o opArities
            if length stack >= arity
                then do
                    let stack' = take arity stack
                    forM_ stack' normalize
                    args <- lift $ mapM (V.read vec) stack'
                    let predIdx = preds !! (arity-1)
                    reductionRule predIdx args o 
                    return False 
            else return True 
        AbstNode -> do
            let predIdx = head preds
            let paramIdx = l outerIdx
            let argIdx = head stack
            let bodyIdx = r outerIdx

            bind bodyIdx paramIdx argIdx
            writeFrom bodyIdx predIdx
            return False

bind :: Int -> Int -> Int -> Reduction ()
bind idx paramIdx argIdx = do
    vec <- gets fst
    VarNode param <- lift $ V.read vec paramIdx
    node <- lift $ V.read vec idx
    
    case node of
        VarNode b ->    when (b == param) $ writeFrom argIdx idx
        AbstNode -> do
            let paramIdx' = l idx
            VarNode param' <- lift $ V.read vec paramIdx'
            unless (param' == param) $ bind (r idx) paramIdx argIdx
        AppNode -> do
            bind (l idx) paramIdx argIdx
            bind (r idx) paramIdx argIdx
        OpNode _ -> return ()
        LitNode _ _ -> return ()
        LetNode v -> do
            if v == param
                then return ()
            else do
                bind (l idx) paramIdx argIdx
                bind (r idx) paramIdx argIdx 

normalize' :: Int -> Reduction Bool
normalize' idx = do
    vec <- gets fst
    (outerIdx, stack, preds) <- lift $ evalStateT (unwind idx vec) ([], [])
    apply outerIdx stack preds
    
normalize :: Int -> Reduction ()
normalize idx = do
    vec <- gets fst
    node <- lift $ V.read vec idx
    case node of 
        VarNode v -> do
            (vidx, whnf) <- gets (fromJust . M.lookup v . snd)
            if whnf
                then writeFrom vidx idx 
            else do
                iterateUntil id (normalize' vidx)
                modify (M.adjust (const (vidx, True)) v <$>)
                writeFrom vidx idx 
        _ -> void $ iterateUntil id (normalize' idx)

-- Built-in operations

valToInt :: Value -> Int
valToInt = read

opArities :: M.Map OpName Int
opArities = M.fromList $ [("+", 2)]

reductionRule :: Int -> [Node] -> OpName -> Reduction ()
reductionRule idx [LitNode _ x, LitNode _ y] "+" = do
    let xInt = valToInt x
    let yInt = valToInt y
    let node = LitNode "Int" (show $ xInt + yInt)
    write idx node 

-- Index helper functions

l :: Int -> Int
l idx = 2*idx+1

r :: Int -> Int
r idx = 2*idx+2

-- Debugging 

debugNode :: Node -> String -> Reduction ()
debugNode node msg | trace (msg ++ show node) True = return ()

-- Testing

li :: Int -> Expr
li val = Literal "Int" (show val)

-- 5 => 5
e1 :: Expr
e1 = li 5

-- 4 + 5 => 9
e2 :: Expr
e2 = App (App (Op "+") (li 4)) (li 5)

-- Let v = 3 in v => 3
e3 :: Expr
e3 = Let "v" (li 3) (Var "v")

-- Let x = 1 + 2 in x + 5 => 8
e4 :: Expr
e4 = Let "x" (App (App (Op "+") (li 1)) (li 2)) $ App (App (Op "+") (Var "x")) (li 5)

-- (\x -> x) 4 => 4
e5 :: Expr
e5 = App (Abst "x" (Var "x")) (li 4)

-- Let id = \x -> x in id 4 => 4
e6 :: Expr
e6 = Let "id" (Abst "x" (Var "x")) (App (Var "id") (li 4))

-- Let f = \x -> 4 in f 3 => 4
e7 :: Expr
e7 = Let "f" (Abst "x" (li 4)) (App (Var "f") (li 3))

-- Let const = \x -> \y -> x in const 1 10 => 1
e8 :: Expr
e8 = Let "const" (Abst "x" (Abst "y" (Var "x"))) (App (App (Var "const") (li 1)) (li 10))

-- Let shadow = \x -> \x -> x in shadow 1 2 => 2
e9 :: Expr
e9 = Let "shadow" (Abst "x" (Abst "x" (Var "x"))) (App (App (Var "shadow") (li 1)) (li 2))

-- Let const = \x -> \y -> x in (Let id = \x -> x in const id 4) => \x -> x
e10 :: Expr
e10 = Let "const" (Abst "x" (Abst "y" (Var "x"))) (Let "id" (Abst "x" (Var "x")) (App (App (Var "const") (Var "id")) (li 4)))

-- e10 6 => 6
e11 :: Expr
e11 = App e10 (li 6)

