{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Eval (eval, invokeMain)
import Lexer (tokenizeStr)
import Parser (parse, parseSingleExpr)
import Result (unwrap)
import Syntax (Format(..))

import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    args <- getArgs
    src <- readFile $ head args
    let defs = unwrap $ tokenizeStr src >>= parse
    handleUserInput defs
    where
        handleUserInput defs = do
            putStr "query: "
            hFlush stdout
            query <- getLine
            if query == "exit" then
                return ()
            else
                let query' = unwrap $ tokenizeStr query >>= parseSingleExpr in
                let result = eval defs query' in
                putStrLn (format result) >> handleUserInput defs
