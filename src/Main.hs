{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Lexer

main :: IO ()
main = do
    print $ tokenizeStr ""
