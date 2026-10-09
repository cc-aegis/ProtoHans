{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Lexer (tokenizeStr)
import Parser (parse)

main :: IO ()
-- main = readFile "../examples/fib.hs" >>= print . (>>= parse) . tokenizeStr
main = print $ (>>= parse) $ tokenizeStr "true :: Int -> Int -> Int = x -> y -> (4)"
