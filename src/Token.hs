module Token (Token(..)) where

data Token = Ident String -- thing
    | TypeName String -- Thing
    | Number String
    | Int
    | World
    | TypeSpec -- ::
    | Arrow
    | Bind -- =
    | Pipe
    | Asterisk
    | DollarSign
    | Minus
    deriving (Eq, Show)
