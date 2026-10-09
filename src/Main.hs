{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Eval (eval, invokeMain)
import Lexer (tokenizeStr)
import Parser (parse)

main :: IO ()
main = do
    let src = "main :: World -> Int = w -> 42"
    let result = tokenizeStr src >>= parse >>= \ defs -> return $ eval defs invokeMain
    print result
