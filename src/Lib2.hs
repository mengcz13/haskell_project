module Lib2 where

import Control.Applicative
import Control.Monad.State
import System.Environment
import System.IO

import Lib

data Option = OptionIo {
    inPath :: String,
    outPath :: String
}
            | OptionStdout {
    inPath :: String
}
            | OptionT {
    tPath :: String,
    outPath :: String
}
            | OptionTStdout {
    tPath :: String
}
    deriving Show

type Parser a = StateT [String] Maybe a

parseFlag :: String -> Parser String
parseFlag f = do
    args <- get
    case args of
        [] -> empty
        (arg : args')
            | arg == "-" ++ f -> do
                put args'
                return f
            | otherwise -> empty
            
parseField :: String -> Parser String
parseField f = do
    parseFlag f
    args <- get
    case args of
        [] -> empty
        (arg : args') -> do
            put args'
            return arg
            
parseInPath :: Parser String
parseInPath = parseField "i"

parseOutPath :: Parser String
parseOutPath = parseField "o"

parseTPath :: Parser String 
parseTPath = parseField "t"

parseOption :: Parser Option
parseOption = p0 <|> p1 <|> p2 <|> p3 where
    p0 = do
        i <- parseInPath
        o <- parseOutPath
        return (OptionIo i o)
        
    p1 = do
        i <- parseInPath
        return (OptionStdout i)

    p2 = do 
        i <- parseTPath
        o <- parseOutPath
        return (OptionT i o)

    p3 = do 
        i <- parseTPath
        return (OptionTStdout i)

processArgs :: (Maybe Option) -> IO()
processArgs Nothing = error "Args Error!"
processArgs (Just op) = processOption op

processOption :: Option -> IO()
processOption (OptionIo inp outp) = do 
    inh <- openFile inp ReadMode
    ouh <- openFile outp WriteMode
    processExpr inh ouh (show.eval)
    hClose inh
    hClose ouh
processOption (OptionStdout inp) = do 
    inh <- openFile inp ReadMode
    processExpr inh stdout (show.eval)
    hClose inh
processOption (OptionT tp outp) = do
    inh <- openFile tp ReadMode
    ouh <- openFile outp WriteMode
    processExpr inh ouh (show.evalTree)
    hClose inh
    hClose ouh
processOption (OptionTStdout tp) = do
    inh <- openFile tp ReadMode
    processExpr inh stdout (show.evalTree)
    hClose inh
processOption _ = error "Args Error!"


processExpr :: Handle -> Handle -> (String -> String) -> IO()
processExpr inh ouh proc = do 
    isEof <- hIsEOF inh
    if isEof
        then return ()
    else do lineStr <- hGetLine inh
            hPutStrLn ouh (proc lineStr)
            processExpr inh ouh proc

defaultMain :: IO ()
defaultMain = do
    args <- getArgs
    processArgs $ evalStateT parseOption args