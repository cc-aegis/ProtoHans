module Parser where

import Ranged (Ranged(..), getStart, getEnd, getContent)
import Result (CompilerError(..), Result(..))
import Syntax
import Token (Token(..))

parse :: [Ranged Token] -> Result Definitions
parse [] = Ok Map.Empty
parse tokens = do
    (def, rest) <- parseDefinition tokens
    defs <- parse rest
    let (name, _, _) = def
    return $ Map.insert (getContent name) def defs

parseDefinition :: [Ranged Token] -> Result (Definition, Ranged Token)
parseDefinition tokens = do
    name <- expectIdent ... TODO



expectIdent :: [Ranged Token] -> Result (Ranged String, Ranged Token)
expectIdent [] = Err Eof
expectIdent ... TODO
