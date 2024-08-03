{-# LANGUAGE TupleSections #-}
module Builder where

import Control.Monad.State
import Data.List (partition, find)
import Text.Megaparsec
import qualified Data.Text.IO as TextIO
import qualified Data.Set as S
import qualified Data.Map as M

import Parser
import GMachine
import LambdaLifting

build :: IO ()
build = do
    input <- TextIO.readFile "src/lib/input.txt"
    let parserOutput = runParser parseProgram "src/lib/input.txt" input
    case parserOutput of
        Left error -> print error
        Right output -> do
            let (parsedSigs, parsedDefs) = partition isTypeSig output
            let defs = map (\(Def (v,expr)) -> (v,expr)) parsedDefs
            let idents = S.fromList $ map fst defs
            
            let liftedDefs' = mapM (\(v,expr) -> (v,) <$> lambdaLift' expr) defs
            let liftedDefs = evalState liftedDefs' (idents, 0)
            
            let scDefs = foldr step opsMap liftedDefs
            let scInst = M.map (\scdef -> (scdef, compile scdef)) scDefs
            
            let mainExpr = find (("main" ==) . fst) defs
            case mainExpr of
                Nothing -> putStrLn "No main function."
                Just (_,me) -> do
                    let mainOutput = evalMain scInst me
                    let finalState = last mainOutput
                    print finalState

    where   step (v, (expr, map')) map = M.insert v (caf expr) (M.union map' map)