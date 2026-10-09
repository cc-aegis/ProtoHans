module Lexer (tokenizeStr) where

import Data.Char (isSpace, isLower, isUpper, isAlphaNum, isDigit)
import Data.Semigroup (Min(..))

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
nextToken (c:cs)
    | getContent c == '\n' && headIsLower cs = Ok (Ranged (pure $ getStart c) (pure $ getEnd c) Token.DefSep, cs)
    | isSpace $ getContent c = nextToken cs
    | isLower $ getContent c = parseIdent (c:cs)
    | isUpper $ getContent c = parseTypeName (c:cs)
    | isDigit $ getContent c = parseNumber (c:cs)
    | otherwise = tryParseOperator (c:cs)
    where
        headIsLower (h:_) = isLower $ getContent h
        headIsLower _ = False

parseIdent :: [Ranged Char] -> Result (Ranged Token, [Ranged Char])
parseIdent src = case sequence ident of
    (Ranged start end "match") -> Ok (Ranged start end Token.Match, rest)
    (Ranged start end "with") -> Ok (Ranged start end Token.With, rest)
    ident' -> Ok (Token.Ident <$> ident', rest)
    where (ident, rest) = span (isAlphaNum . getContent) src

parseTypeName :: [Ranged Char] -> Result (Ranged Token, [Ranged Char])
parseTypeName src = case sequence ident of
    (Ranged start end "Int") -> Ok (Ranged start end Token.Int, rest)
    (Ranged start end "World") -> Ok (Ranged start end Token.World, rest)
    ident' -> Ok (Token.TypeName <$> ident', rest)
    where (ident, rest) = span (isAlphaNum . getContent) src

parseNumber :: [Ranged Char] -> Result (Ranged Token, [Ranged Char])
parseNumber src = Ok (Token.Number <$> sequence ident, rest)
    where (ident, rest) = span (isDigit . getContent) src

tryParseOperator :: [Ranged Char] -> Result (Ranged Token, [Ranged Char])
tryParseOperator [] = Err Eof
tryParseOperator (Ranged start _ ':' : Ranged _ end ':' : rest) = Ok (Ranged start end Token.TypeSpec, rest)
tryParseOperator (Ranged start _ '-' : Ranged _ end '>' : rest) = Ok (Ranged start end Token.Arrow, rest)
tryParseOperator (Ranged start end operator : rest) = do
    token <- case operator of
        '=' -> Ok Token.Bind
        '$' -> Ok Token.DollarSign
        '*' -> Ok Token.Asterisk
        '|' -> Ok Token.Pipe
        '-' -> Ok Token.Minus
        '_' -> Ok Token.Underscore
        '(' -> Ok Token.LParen
        ')' -> Ok Token.RParen
        _ -> Err $ UnexpectedChar (getMin start) operator
    return (Ranged start end token, rest)
