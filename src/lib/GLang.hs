
module GLang where

import Data.Bifunctor (first)
import Data.List (partition)
import Text.Megaparsec
import qualified Data.Text.IO as TextIO
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Text (Text)
import Data.Void (Void)

import Parser
import GMachine
import LowerToGCode
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
        
gmOutputIO :: String -> IO (Either Error Node)
gmOutputIO filename = do
    evalOutput <- evalProgramIO filename
    pure $ gmOutput <$> evalOutput
