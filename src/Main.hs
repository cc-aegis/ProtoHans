{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Lexer (tokenizeStr)

main :: IO ()
main = do
    src <- readFile "examples/fib.hs"
    print $ tokenizeStr src
