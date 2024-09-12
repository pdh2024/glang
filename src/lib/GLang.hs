
module Glang where

import System.IO.Unsafe
import Data.Functor ((<&>))
import Data.Bifunctor (first)
import Data.List (partition)
import Text.Megaparsec
import qualified Data.Text.IO as TextIO
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Text (Text)
import Data.Void (Void)

import Parser
import Gmachine 
import LowerToGcode
import LambdaLifting
import Expression

lowerIO :: String -> IO (Either Error (M.Map VarName (SCDef, [Inst])))
lowerIO filename = do
    parserOutput <- first liftError <$> parseFile filename    
    let partitioned = partition isTypeSig <$> parserOutput
    let parsedSigs = fst <$> partitioned
    let parsedDefs = snd <$> partitioned 
    
    let defs = (fmap . fmap) (\(Def (v,expr)) -> (v,expr)) parsedDefs
    pure $ flip lower opsMap <$> defs
    where   liftError :: ParseErrorBundle Text Void -> Error
            liftError = ParseError . errorBundlePretty
    
evalProgramIO :: String -> IO (Either Error [GmState])
evalProgramIO filename = do
    lowerOutput <- lowerIO filename
    pure $ do
        scInst <- lowerOutput
        evalProgram scInst
    
evalToOutputIO :: String -> IO (Either Error Node)
evalToOutputIO = ((>>= evalToOutput) <$>) . lowerIO
        
-- For testing

evalToOutputFromFile :: String -> Either Error Node
evalToOutputFromFile = unsafePerformIO . ((>>= evalToOutput) <$>) . lowerIO

