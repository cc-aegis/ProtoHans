module Syntax (Definitions, Type(..), AstItem(..), PatItem(..), BinOperator(..)) where

import Data.Map (Map)

type Definitions = Map String Definition

type Definition = (Ranged String, Ranged Type, Ranged AstItem)

data Type = Int | World | Function (Ranged Type) (Ranged Type)

data AstItem = Lambda String AstItem
    | Match (Ranged AstItem) [(Ranged PatItem, Ranged AstItem)]
    | AstInt (Ranged Int)
    | Ivocation (Ranged AstItem) (Ranged AstItem)
    | Binding (Ranged String)
    | BinOp (Ranged BinOperator) (Ranged AstItem) (Ranged AstItem)

data PatItem = PatInt (Ranged Int) | Any

data BinOperator = Sub | Mul | Dollar
