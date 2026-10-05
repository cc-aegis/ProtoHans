module Parser where

import Ranged (Ranged(..), getStart, getEnd, getContent, nest)
import Result (CompilerError(..), Result(..))
import Syntax (Definitions, Definition(..), Type(..), Expr(..), Pat(..), BinOperator(..))
import Token (Token(..))

import qualified Data.Map as Map

parse :: [Ranged Token] -> Result Definitions
parse [] = Ok Map.empty
parse tokens = do
    (def, rest) <- parseDefinition tokens
    defs <- parse rest
    let (Definition name _ _) = def
    return $ Map.insert (getContent name) def defs

parseDefinition :: [Ranged Token] -> Result (Definition, [Ranged Token])
parseDefinition tokens = do
    (name, tokens') <- expectIdent tokens
    tokens'' <- expectToken Token.TypeSpec tokens'
    (type', tokens''') <- parseType tokens''
    tokens'''' <- expectToken Token.Bind tokens'''
    (expr, tokens''''') <- parseExpr tokens''''
    return (Definition name type' expr, tokens''''')

parseType :: [Ranged Token] -> Result (Ranged Type, [Ranged Token])
parseType tokens = do
    (lht, tokens') <- case tokens of
        [] -> Err Eof
        (Ranged start end Token.Int : rest) -> Ok (Ranged start end TyInt, rest)
        (Ranged start end Token.World : rest) -> Ok (Ranged start end TyWorld, rest)
        (rangedToken : _) -> Err $ UnexpectedToken rangedToken
    case tokens' of
        (Ranged _ _ Token.Arrow : tokens'') -> do
            (rht, tokens''') <- parseType tokens''
            Ok (TyFunction <$> nest lht <*> nest rht, tokens''') -- does this work?
        _ -> Ok (lht, tokens')

parseExpr :: [Ranged Token] -> Result (Ranged Expr, [Ranged Token])
parseExpr (Ranged start end (Token.Ident ident) : Ranged _ _ Token.Arrow : tokens) = do
    (expr, tokens') <- parseExpr tokens
    return (Ranged start (pure $ getEnd expr) (ExLambda (Ranged start end ident) expr), tokens')
--parseExpr (Ranged start _ Token.Match : tokens) = do
--    (value, tokens') <- parseExpr tokens
--    tokens'' <- expectToken Token.With tokens'
--    (cases, tokens''') <- parseMatchCases tokens''
--    return TODO
--
--    where
--        parseMatchCases (Ranged _ _ Token.Pipe : tokens) = do
--
--        parseMatchCases _ = Ok []
parseExpr tokens = parseTinyExpr tokens

parseTinyExpr :: [Ranged Token] -> Result (Ranged Expr, [Ranged Token])
parseTinyExpr [] = Err Eof
parseTinyExpr (Ranged start _ Token.LParen : tokens) = do
    (Ranged _ _ body, tokens') <- parseExpr tokens
    (Ranged _ end _, tokens'') <- expectCondToken (==Token.RParen) tokens'
    return (Ranged start end body, tokens'')
parseTinyExpr (Ranged start end (Token.Ident ident) : tokens) =
    return (Ranged start end $ ExBinding (Ranged start end ident), tokens)
parseTinyExpr (Ranged start end (Token.Number number) : tokens) =
    return (Ranged start end $ ExConstant . (Ranged start end) . read $ number, tokens)
parseTinyExpr (token : _) = Err $ UnexpectedToken token

---parseExpr tokens = Ok (pure $ ExConstant (pure 0), tokens) -- PLACEHOLDER

    -- DAS IST BS
--expectIdent :: [Ranged Token] -> Result (Ranged String, Ranged Token)
--expectIdent [] = Err Eof
--expectIdent (token:tokens) = (,tokens) <$> tryMapRanged asIdent token
--    where
--        asIdent (Ident ident) = Ok ident
--        asIdent token' = Err $ UnexpectedToken token'

expectIdent :: [Ranged Token] -> Result (Ranged String, [Ranged Token])
expectIdent [] = Err Eof
expectIdent (Ranged start end (Ident ident) : rest) = Ok (Ranged start end ident, rest)
expectIdent (rangedToken : _) = Err $ UnexpectedToken rangedToken

expectToken :: Token -> [Ranged Token] -> Result [Ranged Token]
expectToken _ [] = Err Eof
expectToken token (token' : tokens)
    | token == getContent token' = Ok tokens
    | otherwise = Err $ UnexpectedToken token'

expectCondToken :: (Token -> Bool) -> [Ranged Token] -> Result (Ranged Token, [Ranged Token])
expectCondToken _ [] = Err Eof
expectCondToken f (t:ts)
    | f $ getContent t = Ok (t, ts)
    | otherwise = Err $ UnexpectedToken t

tryMapRanged :: (a -> Result b) -> Ranged a -> Result (Ranged b)
tryMapRanged f (Ranged start end a) = case f a of
    Ok b -> Ok (Ranged start end b)
    Err e -> Err e
