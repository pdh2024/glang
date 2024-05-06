module GraphReduction where

import Control.Monad.ST
import Data.Array.ST
import Data.Array
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Functor.Identity
import Expression

data Graph = AppNode | ExprNode Expr | Nil deriving (Show, Eq)

opArities :: M.Map OpName Int
opArities = M.fromList $ [("+", 2), ("t", 3)]

literalToInt :: Expr -> Int
literalToInt (Literal _ x) = read x

reductionRule :: OpName -> [Expr] -> Expr
reductionRule "+" args = let [x,y] = (map literalToInt args) in Literal "Int" $ show (x + y)
reductionRule "t" args = let [x,y,z] = (map literalToInt args) in Literal "Int" $ show (x + y + z)

levels :: Expr -> Int
levels (App e1 e2) = 1 + max (levels e1) (levels e2)
levels (Let _ _ e2) = levels e2
levels _ = 0

size :: Expr -> Int
size expr = go (levels expr)
            where   go :: Int -> Int 
                    go 1 = 2
                    go n = 2^n + go (n-1)

reduce :: Expr -> Array Int Graph
reduce expr = runST $ do
    arr <- newArray (0, size expr) Nil :: ST s (STArray s Int Graph)
    map <- execStateT (populate expr 0 arr) (M.empty)
    runReaderT (normalize arr) map
    freeze arr

populate :: Expr -> Int -> STArray s Int Graph -> StateT (M.Map VarName Expr) (ST s) () 
populate (App e1 e2) idx arr = do
    lift $ writeArray arr idx AppNode
    populate e1 (2*idx+1) arr
    populate e2 (2*idx+2) arr

populate (Let v e1 e2) idx arr = do
    map <- get
    put $ M.insert v e1 map
    populate e2 idx arr

populate expr idx arr = do
    lift $ writeArray arr idx (ExprNode expr)

unwind :: Int -> STArray s Int Graph -> ST s (Int, [Graph])
unwind idx arr = go idx [] arr 
                    where   go :: Int -> [Graph] -> STArray s Int Graph -> ST s (Int, [Graph])
                            go idx stack arr = do
                                e <- readArray arr idx
                                case e of
                                    AppNode -> do
                                        right <- readArray arr (2*idx+2)
                                        go (2*idx+1) (right : stack) arr
                                    ExprNode _ -> return (idx, stack)

rewind :: Int -> Int -> Int
rewind idx 0 = idx
rewind idx count = rewind (div (idx - 1) 2) (count - 1) 

nodeToExpr :: Graph -> Expr
nodeToExpr (ExprNode e) = e

removeVars :: M.Map VarName Expr -> Expr -> Expr
removeVars map (Var v) = fromJust $ M.lookup v map
removeVars _ expr = expr

normalize :: STArray s Int Graph -> ReaderT (M.Map VarName Expr) (ST s) ()
normalize arr = do
    map <- ask
    (outerIdx, stack) <- lift $ unwind 0 arr
    whnf <- apply outerIdx stack arr
    if whnf
        then return ()
    else normalize arr

apply :: Int -> [Graph] -> STArray s Int Graph -> ReaderT (M.Map VarName Expr) (ST s) Bool
apply _ [] _ = return True

apply outerIdx stack arr = do
    map <- ask
    outer <- lift $ (removeVars map) <$> nodeToExpr <$> (readArray arr outerIdx)
    case outer of
        Op o -> let arity = (fromJust $ M.lookup o opArities)
                    in  if length stack >= arity
                            then do
                                args <- return $ take arity ((removeVars map) <$> nodeToExpr <$> stack)
                                expr' <- return $ reductionRule o args
                                predIdx <- return $ rewind outerIdx arity
                                lift $ updatePred predIdx expr' arr
                                return False
                        else return True
        Abst param body -> do
            arg <- return $ nodeToExpr $ head stack
            expr' <- instantiate param body arg
            predIdx <- return $ rewind outerIdx 1
            lift $ updatePred predIdx expr' arr
            return False 

instantiate :: VarName -> Expr -> Expr -> ReaderT (M.Map VarName Expr) (ST s) Expr
instantiate param body arg = do
    map <- ask
    case body of
        Var b ->    if b == param
                        then case arg of
                                Var a -> return $ fromJust $ M.lookup a map
                                _ -> return arg
                    else return $ fromJust $ M.lookup b map
        App e1 e2 -> do
            e1' <- instantiate param e1 arg
            e2' <- instantiate param e2 arg
            return $ App e1' e2'
        Abst innerParam innerBody ->    if innerParam == param
                                            then return innerBody
                                        else do
                                            innerBody' <- instantiate param innerBody arg
                                            return $ Abst innerParam innerBody'
        Op _ -> return body
        Literal _ _ -> return body

updatePred :: Int -> Expr -> STArray s Int Graph -> ST s ()
updatePred predIdx expr' arr = evalStateT (populate expr' predIdx arr) M.empty

-- testing

    -- initial populate

e1 :: Expr
e1 = (App (App (Op "+") (Literal "Int" "3")) (Literal "Int" "4"))

e2 :: Expr
e2 = Let "f" (Op "+") (App (App (Var "f") (Literal "Int" "3")) (Literal "Int" "4"))

e3 :: Expr
e3 = Let "f" (Op "t") (App (App (App (Var "f") (Literal "Int" "3")) (Literal "Int" "4")) (Literal "Int" "5"))

size1 :: Int
size1 = size e1

size2 :: Int
size2 = size e2

size3 :: Int
size3 = size e3

a1 :: Array Int Graph
a1 = reduce e1

a2 :: Array Int Graph
a2 = reduce e2

a3 :: Array Int Graph
a3 = reduce e3

    -- unwind

unwindTest :: Expr -> Array Int Graph
unwindTest expr = runST $ do
    arr <- newArray (0, size expr) Nil :: ST s (STArray s Int Graph)
    map <- execStateT (populate expr 0 arr) (M.empty)
    (outerIdx, stack) <- unwind 0 arr
    outer <- readArray arr outerIdx
    writeArray arr 0 outer
    freeze arr

u1 :: Array Int Graph
u1 = unwindTest e1

u2 :: Array Int Graph
u2 = unwindTest e2

    -- rewind test

rewindTest :: Expr -> Array Int Graph
rewindTest expr = runST $ do
    arr <- newArray (0, size expr) Nil :: ST s (STArray s Int Graph)
    map <- execStateT (populate expr 0 arr) (M.empty)
    (outerIdx, stack) <- unwind 0 arr
    outer <- nodeToExpr <$> readArray arr outerIdx
    count <- let outer' = case outer of
                            Var v -> fromJust $ M.lookup v map
                            _ -> outer
                in case outer' of
                    Op o -> return $ (fromJust $ M.lookup o opArities) 
                    Abst _ _ -> return 1
    predIdx <- return $ rewind outerIdx count
    pred <- readArray arr predIdx
    writeArray arr 0 pred
    writeArray arr predIdx Nil
    freeze arr

r1 :: Array Int Graph
r1 = rewindTest e1

r2 :: Array Int Graph
r2 = rewindTest e2

r3 :: Array Int Graph
r3 = rewindTest e3

    -- updatePred 

    -- instantiate

    -- apply 

    -- normalize

    -- reduce









    


