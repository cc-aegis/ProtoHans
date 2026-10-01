module Lexer (tokenize) where

import Data.Char (isSpace)

import Ranged (Ranged)
import Result (CompilerError(..), Result(..))
import Token (Token)

tokenizeStr :: String -> Result [Ranged Token]
tokenizeStr = tokenize . zip [0..]

tokenize :: [Ranged Char] -> Result [Ranged Token]
tokenize [] = Ok []
tokenize src
    | nextToken == Err Eof = Ok []
    | otherwise = do
        (token, src') <- nextToken'
        tokens' <- tokenize src'
        return $ token : tokens
    where nextToken' = nextToken src

isLowercase :: Char -> Bool
isLowercase c = 'a' <= c && c <= 'z'

isUppercase :: Char -> Bool
isUppercase c = 'A' <= c && c <= 'Z'

isIdent :: Char -> Bool
isIdent c = isLowercase c || isUppercase c

nextToken :: [Ranged Char] -> Result (Ranged Token, [Ranged Char])
nextToken [] =  Err $ Eof
nextToken (Ranged { start, content = ':' } : Ranged { end, content = ':' } : rest) =
    Ok (Ranged { start, end, content = Token.TypeSpec }, rest)
nextToken (Ranged { start, content = '-' } : Ranged { end, content = '>' } : rest) =
    Ok (Ranged { start, end, content = Token.Arrow }, rest)
nextToken (Ranged { start, end, content = '=' } : rest) =
    Ok (Ranged { start, end, content = Token.Bind }, rest)
nextToken (Ranged { start, end, content = '$' } : rest) =
    Ok (Ranged { start, end, content = Token.DollarSign }, rest)
nextToken (Ranged { start, end, content = '*' } : rest) =
    Ok (Ranged { start, end, content = Token.Asterisk }, rest)
nextToken (Ranged { start, end, content = '|' } : rest) =
    Ok (Ranged { start, end, content = Token.Pipe }, rest)
nextToken (Ranged { start, end, content = '-' } : rest) =
    Ok (Ranged { start, end, content = Token.Minus }, rest)
nextToken (c:cs)
    | isSpace c = nextToken cs
    | isLowercase $ getContent c = parseIdent (c:cs)
    | isUppercase $ getContent c = parseTypeName (c:cs)
    | otherwise = Err $ UnexpectedChar (getStart c) (getContent c)

parseIdent :: [Ranged Char] -> Result (Ranged Token, [Ranged Char])
parseIdent src = Ok (Ident <$> sequence ident) rest
    where (ident, rest) = span isIdent src

parseTypeName :: [Ranged Char] -> Result (Ranged Token, [Ranged Char])
parseTypeName src = Ok (TypeName <$> sequence ident) rest
    where (ident, rest) = span isIdent src
