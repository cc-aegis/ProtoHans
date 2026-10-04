module Parser where

import Ranged (Ranged(..), getStart, getEnd, getContent, nest)
import Result (CompilerError(..), Result(..))
import Syntax (Definitions, Definition, Type(..), Expr(..), Pat(..), BinOperator(..))
import Token (Token(..))

parse :: [Ranged Token] -> Result Definitions
parse [] = Ok Map.Empty
parse tokens = do
    (def, rest) <- parseDefinition tokens
    defs <- parse rest
    let (name, _, _) = def
    return $ Map.insert (getContent name) def defs

parseDefinition :: [Ranged Token] -> Result (Definition, [Ranged Token])
parseDefinition tokens = do
    (name, tokens') <- expectIdent tokens
    tokens'' <- expectToken TypeSpec
    (type', tokens''') <- parseType tokens''
    tokens'''' <- expectToken Bind
    (expr, tokens''''') <- parseExpr tokens''''
    return (Definition name type' expr, tokens''''')


parseType :: [Ranged Token] -> Result (Type, [Ranged Token])
parseType tokens = do
    (lht, tokens') <- case tokens of
        [] -> Err Eof
        (Ranged start end Token.Int : rest) -> Ok (Ranged start end Type.Int : rest)
        (Ranged start end Token.World : rest) -> Ok (Ranged start end Type.World : rest)
        (rangedToken : _) -> Err $ UnexpectedToken rangedToken
    case tokens' of
        [Ranged _ _ Arrow, token''] -> do
            (rht, tokens''') <- parseType tokens''
            Ok (Type.Function <$> nest lht <*> nest rht, tokens''') -- does this work?
        _ -> Ok (lht, tokens')

parseExpr :: [Ranged Token] -> Result (Expr, [Ranged Token])
parseExpr tokens = Ok (Expr.Constant (pure 0), tokens) -- PLACEHOLDER
--9103

    -- DAS IST BS
--expectIdent :: [Ranged Token] -> Result (Ranged String, Ranged Token)
--expectIdent [] = Err Eof
--expectIdent (token:tokens) = (,tokens) <$> tryMapRanged asIdent token
--    where
--        asIdent (Ident ident) = Ok ident
--        asIdent token' = Err $ UnexpectedToken token'

expectIdent :: [Ranged Token] -> Result (Ranged String, Ranged Token)
expectIdent [] = Err Eof
expectIdent (Ranged start end (Ident ident) : rest) = Ok (Ranged start end ident, rest)
expectIdent (rangedToken : _) = Err $ UnexpectedToken rangedToken

expectToken :: Token -> [Ranged Token] -> Result [Ranged Token]
expectToken _ [] = Err Eof
expectToken token (token' : tokens)
    | token == getContent token' = Ok tokens
    | otherwise = Err $ UnexpectedToken token'

tryMapRanged :: (a -> Result b) -> Ranged a -> Result (Ranged b)
tryMapRanged f (Ranged start end a) = case f a of
    Ok b -> Ok (Ranged start end b)
    Err e -> Err e
