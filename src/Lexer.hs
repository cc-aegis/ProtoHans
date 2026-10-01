module Lexer (tokenizeStr) where

import Data.Char (isSpace)

import Ranged (Ranged(..), getStart, getEnd, getContent)
import Result (CompilerError(..), Result(..))
import Token (Token(..))

tokenizeStr :: String -> Result [Ranged Token]
tokenizeStr = tokenize . (mkRange <$>) . zip [0..]
    where mkRange (i, c) = Ranged (pure i) (pure i) c

tokenize :: [Ranged Char] -> Result [Ranged Token]
tokenize [] = Ok []
tokenize src -- TODO: case of
    | nextToken' == Err Eof = Ok []
    | otherwise = do
        (token, rest) <- nextToken'
        tokens <- tokenize rest
        return $ token : tokens
    where nextToken' = nextToken src

isLowercase :: Char -> Bool
isLowercase c = 'a' <= c && c <= 'z'

isUppercase :: Char -> Bool
isUppercase c = 'A' <= c && c <= 'Z'

isIdent :: Char -> Bool
isIdent = isLowercase ||| isUppercase

nextToken :: [Ranged Char] -> Result (Ranged Token, [Ranged Char])
nextToken [] =  Err Eof
nextToken (Ranged start _ ':' : Ranged _ end ':' : rest) =
    Ok (Ranged start end Token.TypeSpec, rest)
nextToken (Ranged start _ '-' : Ranged _ end '>' : rest) =
    Ok (Ranged start end Token.Arrow, rest)
nextToken (Ranged start end '=' : rest) =
    Ok (Ranged start end Token.Bind, rest)
nextToken (Ranged start end '$' : rest) =
    Ok (Ranged start end Token.DollarSign, rest)
nextToken (Ranged start end '*' : rest) =
    Ok (Ranged start end Token.Asterisk, rest)
nextToken (Ranged start end '|' : rest) =
    Ok (Ranged start end Token.Pipe, rest)
nextToken (Ranged start end '-' : rest) =
    Ok (Ranged start end Token.Minus, rest)
nextToken (c:cs)
    | isSpace $ getContent c = nextToken cs
    | isLowercase $ getContent c = parseIdent (c:cs)
    | isUppercase $ getContent c = parseTypeName (c:cs)
    | otherwise = Err $ UnexpectedChar (getStart c) (getContent c)

parseIdent :: [Ranged Char] -> Result (Ranged Token, [Ranged Char])
parseIdent src = Ok (Token.Ident <$> sequence ident, rest)
    where (ident, rest) = span (isIdent . getContent) src

parseTypeName :: [Ranged Char] -> Result (Ranged Token, [Ranged Char])
parseTypeName src = Ok (Token.TypeName <$> sequence ident, rest)
    where (ident, rest) = span (isIdent . getContent) src
