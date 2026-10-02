module Syntax (Definitions, Type(..), AstItem(..), PatItem(..), BinOperator(..)) where

import Data.Map (Map)

type Definitions = Map String Type AstItem

data Type = Int | World | Function Type Type

data AstItem = Lambda String AstItem
    | Match AstItem [(PatItem, AstItem)]
    | AstInt Int
    | Ivocation AstItem AstItem
    | Binding String
    | BinOp BinOperator AstItem AstItem

data PatItem = PatInt Int | Any

data BinOperator = Sub | Mul | Dollar
