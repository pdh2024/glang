{-# LANGUAGE RecordWildCards #-}

module Gmachine where

import Data.Maybe (fromJust)

import Data.List (find)
import qualified Data.Map as M
import qualified Data.Set as S

import LowerToGcode 
import Expression

-- G-Machine

data Node =
        LitNode Expr
    |   VarNode VarName 
    |   AppNode Int Int
    deriving (Show, Eq)

data GmState = GmState {scInst :: M.Map VarName (SCDef, [Inst]), code :: [Inst], stack :: [Int], heap :: [Node], stdout :: [Node]} 

data Error = ProgramError String | ParseError String deriving (Show, Eq)

instance Show GmState where
    show state@(GmState {..}) = "State:" ++ "\ncode = " ++ show code ++ "\nstack = " ++ show stack ++ "\nheap = " ++ show heap ++ "\n\nInst:" ++ showMap scInst ++ "\n\n"
    
showMap :: (Show k, Show a) => M.Map k a -> String
showMap = M.foldrWithKey (\k a acc -> acc ++ "\n" ++ show k ++ ":\n" ++ show a) "" 

accessHeap :: Int -> [Node] -> Node
accessHeap idx heap = heap !! (length heap - 1 - idx)

-- Instruction implementations

data Impl = Impl {name :: String, exec :: GmState -> GmState} 

instance Show Impl where
    show = name
    
pushLitImpl :: Expr -> Impl
pushLitImpl lit = Impl {name = "pushLit " ++ show lit, exec = \state@(GmState {..}) -> state {stack = length heap : stack, heap = LitNode lit : heap}}

pushImpl :: Int -> Impl
pushImpl i = Impl {name = "push " ++ show i, exec = \state@(GmState {..}) -> let addr = stack !! i in state {stack = addr : stack}}

pushSCImpl :: VarName -> Impl
pushSCImpl v = Impl {name = "pushSC " ++ v, exec = \state@(GmState {..}) -> state {stack = length heap : stack, heap = VarNode v : heap}}

makeAppImpl :: Impl
makeAppImpl = Impl {name = "makeApp", exec = go}
    where   go state@(GmState {..}) =   let (x:y:xs) = stack
                                            node = AppNode x y
                                            in state {stack = length heap : xs, heap = node : heap}
                                            
updateList :: Int -> a -> [a] -> [a]                                            
updateList _ _ [] = []
updateList 0 e (x:xs) = e:xs
updateList idx e (x:xs) = x : updateList (idx-1) e xs

updateImpl :: Int -> Impl
updateImpl i = Impl {name = "update " ++ show i, exec = go}
    where   go state@(GmState {..}) =   let normAddr = head stack
                                            norm = accessHeap normAddr heap
                                            redexAddr = tail stack !! i
                                            heap' = updateList (length heap - 1 - redexAddr) norm heap
                                            in state {stack = tail stack, heap = heap'}
                                            
popImpl :: Int -> Impl                                            
popImpl i = Impl {name = "pop " ++ show i, exec = \state -> state {stack = drop i . stack $ state}}

rearrange :: Int -> [Node] -> [Int] -> [Int]
rearrange n heap (x:xs)
    | n == 1 = addr2 : x : xs 
    | otherwise = addr2 : rearrange (n-1) heap xs
    where   AppNode _ addr2 = accessHeap x heap

unwindImpl :: Impl
unwindImpl = Impl {name = "unwind", exec = go}
    where   go state@(GmState {..}) =   let node = accessHeap (head stack) heap
                                            in  case node of
                                                    LitNode _ -> state
                                                    AppNode addr1 addr2 -> state {code = Unwind : code, stack = addr1 : stack}
                                                    VarNode v ->    let (SCDef vs _, code') = fromJust $ M.lookup v scInst
                                                                        arity = length vs
                                                                            in  if arity == 0
                                                                                    then state {code = code' ++ code, stack = tail stack}
                                                                                else if length (tail stack) >= arity
                                                                                        then    let stack' = rearrange arity heap (tail stack)
                                                                                                    in state {code = code' ++ code, stack = stack'} 
                                                                                else state {code = code ++ [Rewind], stack = tail stack}

rewindImpl :: Impl
rewindImpl = Impl {name = "rewind", exec = go}
    where   go state@(GmState {..}) =   case stack of
                                            (x:y:_) -> state {code = code ++ [MakeApp]}
                                            _ -> state 

-- Generate implementations of strict ops

makeBinop :: (Node -> a) -> (a -> Node) -> String -> (a -> a -> a) -> Impl
makeBinop unbox box ident f = Impl {name = ident, exec = go}
    where   go state@(GmState {..}) =   let (x:y:xs) = stack
                                            xnode = accessHeap x heap
                                            ynode = accessHeap y heap
                                            out = box $ f (unbox xnode) (unbox ynode)
                                            in state {stack = length heap : stack, heap = out : heap}

unboxFloat :: Node -> Float
unboxFloat (LitNode (Literal "Float" val)) = read val

boxFloat :: Float -> Node
boxFloat = LitNode . Literal "Float" . show

unboxBool :: Node -> Bool
unboxBool (LitNode (Literal "Bool" val)) = read val

boxBool :: Bool -> Node
boxBool = LitNode . Literal "Bool" . show

printOp :: Impl
printOp = Impl {name = "print", exec = go}
    where   go state@(GmState {..}) = let node = accessHeap (head stack) heap in state {stack = head stack : stack, stdout = node : stdout }

condOp :: Impl
condOp = Impl {name = "cond", exec = go}
    where   go state@(GmState {..}) =   let (c:x:y:ys) = stack
                                            condNode = accessHeap c heap
                                            condBool = unboxBool condNode
                                            in  if condBool
                                                    then state {stack = x : stack}
                                                else state {stack = y : stack}

equalsOp :: Impl                                            
equalsOp = Impl {name = "==", exec = go}
    where   go state@(GmState {..}) =   let x:y:ys = stack
                                            xNode = accessHeap x heap
                                            yNode = accessHeap y heap
                                            res = LitNode (Literal "Bool" (show $ xNode == yNode))
                                            in state {stack = length heap : stack, heap = res : heap}

-- Evaluation

makeGm :: M.Map VarName (SCDef, [Inst]) -> [Inst] -> GmState
makeGm scInst initCode = GmState {scInst = scInst, code = initCode, stack = [], heap = [], stdout = []}

dispatch :: GmState -> GmState
dispatch state@(GmState {..}) = case code of
                                    (i:is) ->   let impl = case i of
                                                            Push k -> pushImpl k
                                                            PushSC v -> pushSCImpl v
                                                            PushLit e -> pushLitImpl e
                                                            Update k -> updateImpl k
                                                            Pop k -> popImpl k 
                                                            PerformOp o -> snd $ opImpls M.! o
                                                            MakeApp -> makeAppImpl
                                                            Unwind -> unwindImpl
                                                            Rewind -> rewindImpl
                                                in exec impl (state {code = is})
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

evalExpr :: M.Map VarName (SCDef, [Inst]) -> Expr -> [GmState]
evalExpr scInst expr =    let   initCode = compile $ SCDef [] expr
                                initState = makeGm scInst initCode
                                in eval initState

evalProgram :: M.Map VarName (SCDef, [Inst]) -> Either Error [GmState]
evalProgram scInst =    case M.lookup "main" scInst of
                            Just (_, initCode) -> let initState = makeGm scInst initCode in pure $ eval initState
                            Nothing -> Left . ProgramError $ "No main function."
                            
gmOutput :: [GmState] -> Node
gmOutput states =   let finalState = last states
                        outIdx = head . stack $ finalState
                        in accessHeap outIdx (heap finalState)

evalToOutput :: M.Map VarName (SCDef, [Inst]) -> Either Error Node
evalToOutput = (gmOutput <$>) . evalProgram

-- Built-in strict ops

ternopImpls :: (Int, [Impl])
ternopImpls = (3,
    [
        condOp
    ])

binopImpls :: (Int, [Impl])
binopImpls = (2,
    [
        makeBinop unboxFloat boxFloat "+" (+),
        makeBinop unboxFloat boxFloat "-" (-),
        makeBinop unboxFloat boxFloat "*" (*),
        makeBinop unboxFloat boxFloat "/" (/),
        makeBinop unboxBool boxBool "&" (&&),
        makeBinop unboxBool boxBool "|" (||),
        equalsOp
    ])

unopImpls :: (Int, [Impl])
unopImpls = (1,
    [
        printOp
    ])

implsToMap :: (Int, [Impl]) -> M.Map VarName (Int, Impl)
implsToMap (arity, impls) = M.fromList $ map (\i -> (name i, (arity, i))) impls

opImpls :: M.Map VarName (Int, Impl)
opImpls = foldr (\x acc -> implsToMap x `M.union` acc) M.empty [ternopImpls, binopImpls, unopImpls]

opToSC :: Int -> VarName -> SCDef
opToSC arity o = SCDef (show <$> [1..arity]) (Op o)

opsMap :: M.Map VarName SCDef
opsMap = M.mapWithKey (\name (arity, impl) -> opToSC arity name) opImpls
