{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Applicative
import Control.Monad.State
import System.Environment
import Data.Attoparsec.Text
import Data.Functor
import Data.Text

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"

data Expr
    = FalseLit
    | TrueLit
    | Not Expr
    | And Expr Expr
    | Or Expr Expr
    | Number Double
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Equ Expr Expr
    | Le Expr Expr
    | Leq Expr Expr
    | Ge Expr Expr
    | Geq Expr Expr
    deriving Show
    
exprParser :: Parser Expr
exprParser = falseParser <|> trueParser <|> notParser <|> andParser <|> orParser 
    <|> numberParser <|> addParser <|> subParser <|> mulParser <|> divParser 
    <|> equParser <|> leParser <|> leqParser <|> geParser <|> geqParser

falseParser :: Parser Expr
falseParser = lexeme $ string "False" $> FalseLit

trueParser :: Parser Expr
trueParser = lexeme $ string "True" $> TrueLit

notParser :: Parser Expr
notParser = do
    lexeme $ char '('
    lexeme $ string "not"
    expr <- exprParser
    lexeme $ char ')'
    return (Not expr)

andParser :: Parser Expr
andParser = do
    lexeme $ char '('
    lexeme $ string "and"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (And expr1 expr2)

orParser :: Parser Expr
orParser = do
    lexeme $ char '('
    lexeme $ string "or"
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Or expr1 expr2)

numberParser :: Parser Expr
numberParser = do
    skipSpace
    num <- double
    return (Number num)

addParser :: Parser Expr
addParser = do
    lexeme $ char '('
    lexeme $ char '+'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Add expr1 expr2)

subParser :: Parser Expr
subParser = do
    lexeme $ char '('
    lexeme $ char '-'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Sub expr1 expr2)

mulParser :: Parser Expr
mulParser = do
    lexeme $ char '('
    lexeme $ char '*'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Mul expr1 expr2)

divParser :: Parser Expr
divParser = do
    lexeme $ char '('
    lexeme $ char '/'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Div expr1 expr2)

equParser :: Parser Expr
equParser = do
    lexeme $ char '('
    lexeme $ char '='
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Equ expr1 expr2)

leParser :: Parser Expr
leParser = do
    lexeme $ char '('
    lexeme $ char '<'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Le expr1 expr2)

leqParser :: Parser Expr
leqParser = do
    lexeme $ char '('
    lexeme $ string "<="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Leq expr1 expr2)

geParser :: Parser Expr
geParser = do
    lexeme $ char '('
    lexeme $ char '>'
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Ge expr1 expr2)

geqParser :: Parser Expr
geqParser = do
    lexeme $ char '('
    lexeme $ string ">="
    expr1 <- exprParser
    expr2 <- exprParser
    lexeme $ char ')'
    return (Geq expr1 expr2)
    
lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p

data Value
    = BoolValue Bool
    | NumValue Double
    | Infostr String
    deriving Show

vnot :: Value -> Value
vnot (BoolValue v) = BoolValue $ not v
vnot _ = error "Type Error!"

vand :: Value -> Value -> Value
vand (BoolValue v1) (BoolValue v2) = BoolValue (v1 && v2)
vand _ _ = error "Type Error!"

vor :: Value -> Value -> Value
vor (BoolValue v1) (BoolValue v2) = BoolValue (v1 || v2)
vor _ _ = error "Type Error!"

vadd :: Value -> Value -> Value
vadd (NumValue v1) (NumValue v2) = NumValue (v1 + v2)
vadd _ _ = error "Type Error!"

vsub :: Value -> Value -> Value
vsub (NumValue v1) (NumValue v2) = NumValue (v1 - v2)
vsub _ _ = error "Type Error!"

vmul :: Value -> Value -> Value
vmul (NumValue v1) (NumValue v2) = NumValue (v1 * v2)
vmul _ _ = error "Type Error!"

vdiv :: Value -> Value -> Value
vdiv (NumValue v1) (NumValue v2) = NumValue (v1 / v2)
vdiv _ _ = error "Type Error!"

vequ :: Value -> Value -> Value
vequ (NumValue v1) (NumValue v2) = BoolValue (v1 == v2)
vequ _ _ = error "Type Error!"

vle :: Value -> Value -> Value
vle (NumValue v1) (NumValue v2) = BoolValue (v1 < v2)
vle _ _ = error "Type Error!"

vleq :: Value -> Value -> Value
vleq (NumValue v1) (NumValue v2) = BoolValue (v1 <= v2)
vleq _ _ = error "Type Error!"

vge :: Value -> Value -> Value
vge (NumValue v1) (NumValue v2) = BoolValue (v1 > v2)
vge _ _ = error "Type Error!"

vgeq :: Value -> Value -> Value
vgeq (NumValue v1) (NumValue v2) = BoolValue (v1 >= v2)
vgeq _ _ = error "Type Error!"

evalexpr :: Expr -> Value
evalexpr FalseLit = BoolValue False
evalexpr TrueLit = BoolValue True
evalexpr (Number x) = NumValue x
evalexpr (Not expr) = vnot $ evalexpr expr
evalexpr (And expr1 expr2) = vand (evalexpr expr1) (evalexpr expr2)
evalexpr (Or expr1 expr2) = vor (evalexpr expr1) (evalexpr expr2)
evalexpr (Add expr1 expr2) = vadd (evalexpr expr1) (evalexpr expr2)
evalexpr (Sub expr1 expr2) = vsub (evalexpr expr1) (evalexpr expr2)
evalexpr (Mul expr1 expr2) = vmul (evalexpr expr1) (evalexpr expr2)
evalexpr (Div expr1 expr2) = vdiv (evalexpr expr1) (evalexpr expr2)
evalexpr (Equ expr1 expr2) = vequ (evalexpr expr1) (evalexpr expr2)
evalexpr (Le expr1 expr2) = vle (evalexpr expr1) (evalexpr expr2)
evalexpr (Leq expr1 expr2) = vleq (evalexpr expr1) (evalexpr expr2)
evalexpr (Ge expr1 expr2) = vge (evalexpr expr1) (evalexpr expr2)
evalexpr (Geq expr1 expr2) = vgeq (evalexpr expr1) (evalexpr expr2)

evalentry :: Either String Expr -> Value
evalentry (Left _) = error "Parse Error!"
evalentry (Right expr) = evalexpr expr

eval :: String -> Value
eval str = evalentry $ parseOnly (exprParser <* endOfInput) $ pack str

evalTree str = parseOnly (exprParser <* endOfInput) $ pack str

defaultMain :: IO()
defaultMain = do 
    line <- getLine
    putStrLn $ show $ eval line