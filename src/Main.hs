{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Lexer (tokenizeStr)

main :: IO ()
main = do
    print $ tokenizeStr "idk :: List a -> a"
