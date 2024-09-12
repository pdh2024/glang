module Main where

import System.Environment
import Gmachine
import Glang
import Expression

main :: IO ()
main = do
    a <- getArgs
    let filepath = head a
    out <- evalProgramIO filepath
    case out of
        Left error -> print error
        Right states -> do
            let finalState = last states
            let toStdout = stdout finalState
            mapM_ printNode toStdout
    where   printNode (LitNode (Literal _ v)) = print v
            printNode _ = putStrLn "Exception -- cannot print non-literal."

