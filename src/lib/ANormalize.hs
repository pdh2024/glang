module ANormalize where

import qualified Data.Map as M
import Control.Monad.State

type Value = String
type VarName = String

makeVar :: Int -> String
makeVar k = "__v" ++ show k

data Expr =
        Literal Value
    |   Abst VarName Expr
    |   App Expr Expr
    |   Let VarName Expr Expr
    deriving (Show, Eq)

isAtomic :: Expr -> Bool
isAtomic (App _ _) = False
isAtomic (Let _ _ expr) = isAtomic expr
isAtomic _ = True

normalize :: Expr -> State Int Expr
normalize (Literal v) = return $ Literal v
normalize (Abst v expr) = do
    out <- normalize expr
    return $ Abst v out
normalize (App e1 e2) = do
    if isAtomic e2
        then return $ App e1 e2
    else do
        k <- get
        newVar <- return $ makeVar $ k+1
        put $ k+1 
        out <- normalize e2
        return $ Let newVar out (App e1 e2)

-- unclear how to deal with let-bindings and identifiers here ... to do



    