{-# LANGUAGE RankNTypes #-}

module GraphReduction where

import Control.Monad.ST
import Data.Array.ST
import Data.Array
import Control.Monad (liftM2)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Functor.Identity
import Expression

data Node = AppNode | ExprNode Expr | Nil deriving (Show, Eq)

graphToExpr :: Int -> STArray s Int Node -> ST s Expr
graphToExpr idx arr = do
    elem <- readArray arr idx
    case elem of
        AppNode -> liftM2 App (graphToExpr (2*idx+1) arr) (graphToExpr (2*idx+2) arr)
        ExprNode e -> return e

opArities :: M.Map OpName Int
opArities = M.fromList [("+", 2), ("t", 3), ("*", 2)]

literalToInt :: Expr -> Int
literalToInt (Literal _ x) = read x

reductionRule :: OpName -> [Expr] -> Expr
reductionRule "+" args = let [x,y] = map literalToInt args in Literal "Int" $ show (x + y)
reductionRule "t" args = let [x,y,z] = map literalToInt args in Literal "Int" $ show (x + y + z)
reductionRule "*" args = let [x,y] = map literalToInt args in Literal "Int" $ show (x * y)

levels :: Expr -> Int
levels (App e1 e2) = 1 + max (levels e1) (levels e2)
levels (Let _ _ e2) = levels e2
levels _ = 0

size :: Expr -> Int
size expr = go (levels expr)
            where   go :: Int -> Int 
                    go 1 = 2
                    go n = 2^n + go (n-1)

type Reduction = forall s. StateT (M.Map VarName (Expr, Bool)) (ST s) Expr

reduce' :: Maybe VarName -> Expr -> Reduction
reduce' var expr = do
    map <- get
    arr <- lift (newArray (0, size expr) Nil :: ST s (STArray s Int Node))
    populate expr 0 arr
    normalize arr
    expr' <- lift $ graphToExpr 0 arr
    case var of
        Nothing -> return expr'
        Just v -> do
            put $ M.adjust (const (expr', True)) v map
            return expr'

reduce :: Expr -> Reduction
reduce (Var v) = do
    map <- get
    let (expr, whnf) = fromJust $ M.lookup v map
    if whnf
        then return expr
    else reduce' (Just v) expr
reduce expr@(App _ _) = reduce' Nothing expr
reduce expr@(Let {}) = reduce' Nothing expr

reduce expr = return expr

reduceTL :: M.Map VarName (Expr, Bool) -> Expr -> Expr
reduceTL map expr = runST $ evalStateT (reduce expr) map

populate :: Expr -> Int -> STArray s Int Node -> StateT (M.Map VarName (Expr, Bool)) (ST s) () 
populate (App e1 e2) idx arr = do
    lift $ writeArray arr idx AppNode
    populate e1 (2*idx+1) arr
    populate e2 (2*idx+2) arr

populate (Let v e1 e2) idx arr = do
    map <- get
    put $ M.insert v (e1, False) map
    populate e2 idx arr

populate expr idx arr = do
    lift $ writeArray arr idx (ExprNode expr)

unwind :: Int -> STArray s Int Node -> ST s (Int, [Int])
unwind idx arr = go idx [] arr 
                    where   go :: Int -> [Int] -> STArray s Int Node -> ST s (Int, [Int])
                            go idx stack arr = do
                                e <- readArray arr idx
                                case e of
                                    AppNode -> do
                                        go (2*idx+1) ((2*idx+2) : stack) arr
                                    ExprNode _ -> return (idx, stack)

rewind :: Int -> Int -> Int
rewind idx 0 = idx
rewind idx count = rewind (div (idx - 1) 2) (count - 1) 

normalize :: STArray s Int Node -> StateT (M.Map VarName (Expr, Bool)) (ST s) ()
normalize arr = do
    map <- get
    (outerIdx, stack) <- lift $ unwind 0 arr
    whnf <- apply outerIdx stack arr
    if whnf
        then return ()
    else normalize arr

apply :: Int -> [Int] -> STArray s Int Node -> StateT (M.Map VarName (Expr, Bool)) (ST s) Bool
apply _ [] _ = return True

apply outerIdx stack arr = do
    map <- get
    outer <- lift $ graphToExpr outerIdx arr
    let outer' = case outer of
                    Var v -> fst . fromJust $ M.lookup v map
                    _ -> outer
    case outer' of
        Op o -> let arity = (fromJust $ M.lookup o opArities)
                    in  if length stack >= arity
                            then do
                                let go = flip graphToExpr $ arr
                                args <- lift $ mapM go (take arity stack)
                                args' <- mapM reduce args
                                let expr' = reductionRule o args'
                                let predIdx = rewind outerIdx arity
                                lift $ updatePred predIdx expr' arr
                                return False
                        else return True
        Abst param body -> do
            let go = flip graphToExpr $ arr
            arg <- lift $ go $ head stack
            expr' <- lift $ runReaderT (instantiate param body arg) map
            let predIdx = rewind outerIdx 1
            lift $ updatePred predIdx expr' arr
            return False 

instantiate :: VarName -> Expr -> Expr -> ReaderT (M.Map VarName (Expr, Bool)) (ST s) Expr
instantiate param body arg = do
    map <- ask
    case body of
        Var b ->    if b == param
                        then return arg
                    else return $ fst . fromJust $ M.lookup b map
        App e1 e2 -> liftM2 App (instantiate param e1 arg) (instantiate param e2 arg)
        Abst innerParam innerBody ->    if innerParam == param
                                            then return innerBody
                                        else do
                                            innerBody' <- instantiate param innerBody arg
                                            return $ Abst innerParam innerBody'
        Op _ -> return body
        Literal _ _ -> return body

updatePred :: Int -> Expr -> STArray s Int Node -> ST s ()
updatePred predIdx expr' arr = evalStateT (populate expr' predIdx arr) M.empty

-- Testing --

-- Built-in

li :: Value -> Expr
li = Literal "Int"

b1 :: Expr
b1 = App (App (Op "+") (li "3")) (li "4")

b2 :: Expr
b2 = Let "f" (Op "+") (App (App (Var "f") (li "3")) (li "4"))

b3 :: Expr
b3 = Let "f" (Op "t") (App (App (App (Var "f") (li "3")) (li "4")) (li "5"))

b4 :: Expr
b4 = App (App (Op "+") (App (App (Op "+") (li "3")) (li "4"))) (li "5")

b5 :: Expr
b5 = Let "x" (App (App (Op "+") (li "3")) (li "4")) (App (App (Op "+") (Var "x")) (li "2"))

b6 :: Expr
b6 = App (App (Op "*") (li "4")) (li "5")

-- Abstractions

a1 :: Expr
a1 = Abst "x" (App (App (Op "*") (Var "x")) (Var "x"))

a1' :: Expr
a1' = App a1 (li "3")


-- Outputs -- 

bo1 :: Expr
bo1 = reduceTL M.empty b1

bo2 :: Expr
bo2 = reduceTL M.empty b2

bo3 :: Expr
bo3 = reduceTL M.empty b3

bo4 :: Expr
bo4 = reduceTL M.empty b4

bo5 :: Expr
bo5 = reduceTL M.empty b5

bo6 :: Expr
bo6 = reduceTL M.empty b6

ao1 :: Expr
ao1 = reduceTL M.empty a1'

