module Types.Core where
import Expression

data Type = 
        Prim TypeName
    |   TypeVar VarName
    |   TypeCons TypeName [Type] 
    deriving (Show, Eq, Ord)

data Scheme = Scheme [VarName] Type deriving (Show, Eq)

newtype TypeError = TypeError String deriving (Show, Eq)

mismatch :: Type -> Type -> TypeError
mismatch t1 t2 = TypeError $ "Cannot match type " ++ show t1 ++ " with type " ++ show t2 ++ "."