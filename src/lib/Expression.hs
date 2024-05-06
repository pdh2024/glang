module Expression where

type TypeName = String
type Value = String
type VarName = String
type OpName = String

data Expr = 
        Literal TypeName Value
    |   Var VarName
    |   Abst VarName Expr
    |   Op OpName
    |   App Expr Expr
    |   Let VarName Expr Expr
    deriving (Show, Eq)