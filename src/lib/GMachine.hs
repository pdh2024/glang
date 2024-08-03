{-# LANGUAGE RecordWildCards #-}

module GMachine where

import Data.List (intersperse)
import Data.Maybe (fromJust)

import qualified Data.Map as M
import qualified Data.Set as S

import Expression

-- G-Machine

data Node =
        LitNode Expr
    |   VarNode VarName 
    |   AppNode Int Int
    deriving (Show, Eq)

data GmState = GmState {scInst :: M.Map VarName (SCDef, [Inst]), code :: [Inst], stack :: [Int], heap :: [Node]} 

instance Show GmState where
    show state@(GmState {..}) = "State:" ++ "\ncode = " ++ show code ++ "\nstack = " ++ show stack ++ "\nheap = " ++ show heap ++ "\n\nInst:" ++ printMap scInst ++ "\n\n"
    
printMap :: (Show k, Show a) => M.Map k a -> String
printMap = M.foldrWithKey (\k a acc -> acc ++ "\n" ++ show k ++ ":\n" ++ show a) "" 

accessHeap :: Int -> [Node] -> Node
accessHeap idx heap = heap !! (length heap - 1 - idx)

-- Instructions

data Inst = Inst {name :: String, exec :: GmState -> GmState} 

instance Show Inst where
    show = name
    
pushLit :: Expr -> Inst
pushLit lit = Inst {name = "pushLit " ++ show lit, exec = \state@(GmState {..}) -> state {stack = length heap : stack, heap = LitNode lit : heap}}

push :: Int -> Inst
push i = Inst {name = "push " ++ show i, exec = \state@(GmState {..}) -> let addr = stack !! i in state {stack = addr : stack}}

pushSC :: VarName -> Inst
pushSC v = Inst {name = "pushSC " ++ v, exec = \state@(GmState {..}) -> state {stack = length heap : stack, heap = VarNode v : heap}}

makeApp :: Inst
makeApp = Inst {name = "makeApp", exec = go}
    where   go state@(GmState {..}) =   let (x:y:xs) = stack
                                            node = AppNode x y
                                            in state {stack = length heap : xs, heap = node : heap}
                                            
updateList :: Int -> a -> [a] -> [a]                                            
updateList _ _ [] = []
updateList 0 e (x:xs) = e:xs
updateList idx e (x:xs) = x : updateList (idx-1) e xs

update :: Int -> Inst
update i = Inst {name = "update " ++ show i, exec = go}
    where   go state@(GmState {..}) =   let normAddr = head stack
                                            norm = accessHeap normAddr heap
                                            redexAddr = tail stack !! i
                                            heap' = updateList (length heap - 1 - redexAddr) norm heap
                                            in state {stack = tail stack, heap = heap'}
                                            
pop :: Int -> Inst                                            
pop i = Inst {name = "pop " ++ show i, exec = \state -> state {stack = drop i . stack $ state}}

rearrange :: Int -> [Node] -> [Int] -> [Int]
rearrange n heap (x:xs)
    | n == 1 = addr2 : x : xs 
    | otherwise = addr2 : rearrange (n-1) heap xs
    where   AppNode _ addr2 = accessHeap x heap

unwind :: Inst
unwind = Inst {name = "unwind", exec = go}
    where   go state@(GmState {..}) =   let node = accessHeap (head stack) heap
                                            in  case node of
                                                    LitNode _ -> state
                                                    AppNode addr1 addr2 -> state {code = unwind : code, stack = addr1 : stack}
                                                    VarNode v ->    let (SCDef vs _, code') = fromJust $ M.lookup v scInst
                                                                        arity = length vs
                                                                            in  if arity == 0
                                                                                    then state {code = code' ++ code, stack = tail stack}
                                                                                else if length (tail stack) >= arity
                                                                                        then    let stack' = rearrange arity heap (tail stack)
                                                                                                    in state {code = code' ++ code, stack = stack'} 
                                                                                else state {code = code ++ [rewind], stack = tail stack}

rewind :: Inst
rewind = Inst {name = "rewind", exec = go}
    where   go state@(GmState {..}) =   case stack of
                                            (x:y:_) -> state {code = code ++ [makeApp]}
                                            _ -> state 

-- Strict operations

makeBinop :: (Node -> a) -> (a -> Node) -> String -> (a -> a -> a) -> Inst
makeBinop unbox box ident f = Inst {name = "binop " ++ ident, exec = go}
    where   go state@(GmState {..}) =   let (x:y:xs) = stack
                                            xnode = accessHeap x heap
                                            ynode = accessHeap y heap
                                            out = box $ f (unbox xnode) (unbox ynode)
                                            in state {stack = length heap : stack, heap = out : heap}

unboxFloat :: Node -> Float
unboxFloat (LitNode (Literal "Float" val)) = read val

boxFloat :: Float -> Node
boxFloat = LitNode . Literal "Float" . show

-- Compilation

compile' :: M.Map VarName Int -> Expr -> [Inst]
compile' _ lit@(Literal _ _) = [pushLit lit]
compile' env (Op o) = concatMap normalizeArg [0..(length env - 1)] ++ [fromJust $ M.lookup o opInst] 
compile' env (Var v) =  if M.member v env
                            then [push $ fromJust $ M.lookup v env]
                        else [pushSC v]
compile' env (App e1 e2) = compile' env e2 ++ compile' env' e1 ++ [makeApp]
    where   env' = M.map (+1) env
    
normalizeArg :: Int -> [Inst]
normalizeArg i = [push i, unwind, update i]
    
compile :: SCDef -> [Inst]
compile (SCDef vs body) =   let compiledBody = compile' (M.fromList $ zip vs [0..]) body
                                arity = length vs
                                in  if null vs
                                        then compiledBody ++ [unwind]
                                    else compiledBody ++ [update arity, pop arity, unwind]

-- Evaluation

makeGm :: M.Map VarName (SCDef, [Inst]) -> [Inst] -> GmState
makeGm scInst code = GmState {scInst = scInst, code = code, stack = [], heap = []}

dispatch :: GmState -> GmState
dispatch state@(GmState {..}) = case code of
                                    (i:is) -> exec i $ state {code = is}
                                    _ -> state

final :: GmState -> Bool
final = null . code

takeWhile' :: (a -> Bool) -> [a] -> [a]                      
takeWhile' _ [] = []
takeWhile' _ [x] = [x]
takeWhile' pred (x:y:xs) =  if pred x
                                then    if pred y
                                            then x : y : takeWhile' pred xs
                                        else x : [y]
                            else [x]

eval :: GmState -> [GmState]
eval = takeWhile' (not . final) . iterate dispatch

evalMain ::  M.Map VarName (SCDef, [Inst]) -> Expr -> [GmState]
evalMain scInst expr = let  initCode = compile (SCDef [] expr)
                            initState = makeGm scInst initCode
                            in eval initState

-- Built-in strict operations

opInst :: M.Map VarName Inst
opInst = M.fromList  
    [
    ("+", makeBinop unboxFloat boxFloat "+" (+)), 
    ("-", makeBinop unboxFloat boxFloat "-" (-)), 
    ("*", makeBinop unboxFloat boxFloat "*" (*)),
    ("/", makeBinop unboxFloat boxFloat "/" (/))
    ]

opsMap :: M.Map VarName SCDef
opsMap = M.fromList [("+", addSC), ("-", subSC), ("*", multSC), ("/", divSC)]

addSC :: SCDef
addSC = SCDef ["x", "y"] (Op "+")

subSC :: SCDef
subSC = SCDef ["x", "y"] (Op "-")

multSC :: SCDef
multSC = SCDef ["x", "y"] (Op "*")

divSC :: SCDef
divSC = SCDef ["x", "y"] (Op "/")
