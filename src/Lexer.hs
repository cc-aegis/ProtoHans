module Lexer (tokenizeStr) where

import Data.Char (isSpace, isLower, isUpper, isAlphaNum, isDigit)

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

nextToken :: [Ranged Char] -> Result (Ranged Token, [Ranged Char])
nextToken [] =  Err Eof
-- TODO: parse Operators in seperate function at end and combine all single-character cases
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
nextToken (Ranged start end '_' : rest) =
    Ok (Ranged start end Token.Underscore, rest)
nextToken (Ranged start end '(' : rest) =
    Ok (Ranged start end Token.LParen, rest)
nextToken (Ranged start end ')' : rest) =
    Ok (Ranged start end Token.RParen, rest)
nextToken (c:cs)
    | isSpace $ getContent c = nextToken cs
    | isLower $ getContent c = parseIdent (c:cs)
    | isUpper $ getContent c = parseTypeName (c:cs)
    | isDigit $ getContent c = parseNumber (c:cs)
    | otherwise = Err $ UnexpectedChar (getStart c) (getContent c)

parseIdent :: [Ranged Char] -> Result (Ranged Token, [Ranged Char])
parseIdent src = Ok (Token.Ident <$> sequence ident, rest)
    where (ident, rest) = span (isAlphaNum . getContent) src

parseTypeName :: [Ranged Char] -> Result (Ranged Token, [Ranged Char])
parseTypeName src = Ok (Token.TypeName <$> sequence ident, rest)
    where (ident, rest) = span (isAlphaNum . getContent) src

parseNumber :: [Ranged Char] -> Result (Ranged Token, [Ranged Char])
parseNumber src = Ok (Token.Number <$> sequence ident, rest)
    where (ident, rest) = span (isDigit . getContent) src
