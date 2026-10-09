{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Lexer (tokenizeStr)
import Parser (parse)

main :: IO ()
main = readFile "examples/fib_lisp.hs" >>= print . (>>= parse) . tokenizeStr
-- main = print $ (>>= parse) $ tokenizeStr "even :: Int -> Int = x -> match x with | 0 = 1 | 1 = 0 | _ = even (sub x 2)"
