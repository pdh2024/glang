{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Read (readMaybe)
import Control.Applicative (liftA2)
import Data.List (foldl1')
import Control.Monad.State
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Void 
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified Text.Megaparsec.Char.Lexer as L

import Expression

type Parser = Parsec Void Text

-- Dummy datatype for types

data Type =
        Prim VarName 
    |   TypeCons VarName [Type]
    |   TypeVar VarName
    deriving (Show, Eq)
    
prims :: [VarName]
prims = []

-- Keywords

kw :: [String]
kw = ["let" ,"in"]

-- Helper parsers and combinators -- 

-- Given a list of parsers, returns a parser that tries each one sequentially, and backtracks upon failure 
choice' :: [Parser a] -> Parser a
choice' = foldl1' ((<|>) . try)

-- Consume whitespace and comments

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Helpers for operation parser -- 

prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix opName f = Prefix $ f <$ symbol opName

negate :: Operator Parser Expr
negate = prefix "-" (App (Var "negate"))

sin :: Operator Parser Expr
sin = prefix "sin" (App (Var "sin"))

cos :: Operator Parser Expr
cos = prefix "cos" (App (Var "cos"))

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary opName f = InfixL $ f <$ symbol opName

add :: Operator Parser Expr
add = binary "+" (App . App (Var "+"))

subtract :: Operator Parser Expr
subtract = binary "-" (App . App (Var "-"))

multiply :: Operator Parser Expr
multiply = binary "*" (App . App (Var "*"))

divide :: Operator Parser Expr
divide = binary "/" (App . App (Var "/"))

and :: Operator Parser Expr
and = binary "&" (App . App (Var "&"))

or  :: Operator Parser Expr
or = binary "|" (App . App (Var "|"))

equals :: Operator Parser Expr
equals = binary "==" (App . App (Var "=="))

operatorTable :: [[Operator Parser Expr]]
operatorTable = [[equals], [Parser.and], [Parser.or], [Parser.negate, Parser.sin, Parser.cos], [multiply, divide], [add, Parser.subtract]]

parseTerm :: Parser Expr
parseTerm = choice' [parseAbst, parseLet, parseApp, parseLiteral, parseVar, sc *> parens parseExpr]

-- Expression parsers --

parseLiteralNum :: Parser Expr
parseLiteralNum = Literal "Float" . show <$> ((try . lexeme $ L.float) <|> lexeme L.decimal)

parseLiteralBool :: Parser Expr
parseLiteralBool = Literal "Bool" . T.unpack <$> (symbol "True" <|> symbol "False")

parseLiteralStr' :: Parser Text
parseLiteralStr' = lexeme $ T.pack <$> go where go = char '\"' >> manyTill L.charLiteral (char '\"')

parseLiteralStr :: Parser Expr
parseLiteralStr = Literal "String" . T.unpack <$> parseLiteralStr'

parseLiteral :: Parser Expr
parseLiteral = parseLiteralNum <|> parseLiteralBool -- <|> parseLiteralStr

parseVar :: Parser Expr
parseVar = Var . T.unpack <$> try (parseIdent <* ((void . lookAhead . symbol $ "==") <|> notFollowedBy (char ':' <|> char '=')))

parseAbst :: Parser Expr
parseAbst = do
    Var param <- parseVar
    char '.'
    Abst param <$> parseExpr

parseOp :: Parser Expr
parseOp = makeExprParser parseTerm operatorTable

parseApp :: Parser Expr
parseApp = do
    x <- choice [parens parseExpr, parseVar]
    xs <- many $ choice [parens parseExpr, parseLiteral, parseVar]
    case xs of
        [] -> fail "No argument applied to function."
        _ -> return $ foldl1' App (x:xs)

parseLet :: Parser Expr
parseLet = do
    symbol "let"
    (x, e1) <- parseDef
    symbol "in"
    Let x e1 <$> parseExpr

parseExpr :: Parser Expr
parseExpr = choice' [parseAbst, parseLet, parseOp, parseApp, parseLiteral, parseVar, sc *> parens parseExpr]

-- Core parsers --

parseIdent' :: Parser String
parseIdent' = do
    id <- liftA2 (:) (letterChar <|> char '_' <|> char '\'') (many $ alphaNumChar <|> char '_' <|> char '\'')
    if id `elem` kw
        then fail "Cannot use keyword as identifier."
    else return id

parseIdent :: Parser Text
parseIdent = sc *> (lexeme . (T.pack <$>)) parseIdent'

parseTypeSig' :: Parser Type
parseTypeSig' = do
    x <- parens parseTypeSig' <|> lexeme (Prim . T.unpack <$> choice' (string . T.pack <$> prims)) <|> (TypeVar . T.unpack <$> parseIdent) 
    y <- optional . try $ do
        symbol "->" *> parseTypeSig'
    case y of
        Just y -> return $ TypeCons "->" [x,y]
        Nothing -> return x

parseTypeSig :: Parser (VarName, Type)
parseTypeSig = do
    v <- parseIdent
    symbol ":"
    t <- parseTypeSig'
    return (T.unpack v, t)

parseDef :: Parser (VarName, Expr)
parseDef = do
    x <- parseIdent
    symbol "="
    expr <- parseExpr
    return (T.unpack x, expr)

-- Program parser --

data ParserResult = TypeSig (VarName, Type) | Def (VarName, Expr) deriving Show

parseBlock :: Parser ParserResult
parseBlock = choice' [TypeSig <$> parseTypeSig, Def <$> parseDef]

parseProgram :: Parser [ParserResult]
parseProgram = many parseBlock

isTypeSig :: ParserResult -> Bool
isTypeSig (TypeSig _) = True
isTypeSig _ = False
    
-- Parsing a source file

parseFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) [ParserResult])
parseFile filename = do
    input <- TextIO.readFile filename
    pure $ runParser parseProgram filename input

-- For unit tests

isParserError :: Either (ParseErrorBundle s e) a -> Bool
isParserError (Left _) = True
isParserError _ = False



