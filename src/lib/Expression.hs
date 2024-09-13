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
    |   Let VarName Expr Expr
    |   App Expr Expr
    deriving (Show, Eq)

data SCDef = SCDef [VarName] Expr deriving (Show, Eq)