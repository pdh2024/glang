module PatternMatching where

import Control.Monad.Reader (Reader, runReader, asks)
import Data.Maybe (fromMaybe)

import qualified Data.Map as M

-- Type aliases for the names of variables, constructors, and types

type VarName = String
type ConsName = String
type TypeName = String

-- Datatype for patterns

data Pattern = Var VarName | Cons ConsName [Pattern] deriving (Eq, Show)

-- Dummy datatype for expressions and clauses

data Expr = 
        Expr VarName
    |   Fatbar Expr Expr
    |   Case VarName [Clause]
    deriving (Eq, Show)                     

data Clause = Clause ConsName [VarName] Expr deriving (Eq, Show)

-- Datatype for equations

data Equation = Eq ([Pattern], Expr) deriving (Eq, Show)

-- Type environment

data TypeEnv = TypeEnv
    {
        consToArity :: M.Map ConsName Int,
        consToType :: M.Map ConsName TypeName,
        typeToCons :: M.Map TypeName [ConsName]
    }

-- Helper methods

makeVar :: Int -> VarName
makeVar i = "__u" ++ show i

prefixVar :: Equation -> Bool
prefixVar (Eq (Var v : _, _)) = True
prefixVar _ = False

getCons :: Equation -> ConsName
getCons (Eq (Cons c _ : _, _)) = c

choose :: ConsName -> [Equation] -> [Equation]
choose c = filter ((== c) . getCons)

arity :: ConsName -> M.Map ConsName Int -> Int
arity c consToArity = fromMaybe 0 $ M.lookup c consToArity

-- Helper method for lists

partContiguous :: (a -> Bool) -> [a] -> [[a]]
partContiguous _ [] = []
partContiguous _ [x] = [[x]]
partContiguous p (x:xs) = if p x == (p . head) x' then (x : x') : xs' else [x] : x' : xs'
                            where (x':xs') = partContiguous p xs

-- Substitution for dummy expressions

subst :: VarName -> VarName -> Expr -> Expr
subst old new (Expr v) = if v == old then Expr new else Expr v

-- Match, empty rule, and mixture rule

match :: Int -> [VarName] -> [Equation] -> Reader TypeEnv Expr -> Reader TypeEnv Expr
match _ [] qs def = do
    defexpr <- def
    return $ foldr Fatbar defexpr [e | Eq (ps,e) <- qs]

match k us qs def = foldr (applyRule k us) def pqs
                    where   pqs = partContiguous prefixVar qs
                            applyRule k us qs def = if prefixVar . head $ qs then matchVar k us qs def else matchCons k us qs def

-- Variable rule

matchVar :: Int -> [VarName] -> [Equation] -> Reader TypeEnv Expr -> Reader TypeEnv Expr
matchVar k (u:us) qs def = match k us qs' def
                        where qs' = [Eq (ps, e) | Eq (Var v : ps, e) <- qs]

-- Constructor rule

makeClause :: Int -> ConsName -> [VarName] -> [Equation] -> Reader TypeEnv Expr -> Reader TypeEnv Clause
makeClause k c us qs def = do
    cta_map <- asks consToArity
    let k' = arity c cta_map
        us' = [makeVar (k+i) | i <- [1..k']]
        qs' = [Eq (ps' ++ ps, e) | Eq (Cons c ps' : ps, e) <- qs]
        expr = match (k+k') (us' ++ us) qs' def
        in (Clause c us') <$> expr

matchCons :: Int -> [VarName] -> [Equation] -> Reader TypeEnv Expr -> Reader TypeEnv Expr
matchCons k (u:us) qs def = do
    ctt_map <- asks consToType
    ttc_map <- asks typeToCons
    let t = fromMaybe "" $ M.lookup (getCons . head $ qs) ctt_map
        cs = fromMaybe [] $ M.lookup t ttc_map
        clauses = [makeClause k c us (choose c qs) def | c <- cs, (choose c qs) /= []]
        in (Case u) <$> sequenceA clauses





