{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Lexer (tokenizeStr)

main :: IO ()
main = readFile "examples/fib.hs" >>= print . tokenizeStr
