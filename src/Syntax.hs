module Syntax (Definitions, Definition, Type(..), Expr(..), Pat(..), BinOperator(..)) where

import Ranged (Ranged)

import Data.Map (Map)

type Definitions = Map String Definition

data Definition = Definition (Ranged String) (Ranged Type) (Ranged Expr)

data Type = TyInt | TyWorld | TyFunction (Ranged Type) (Ranged Type)

data Expr = Lambda String Expr
    | Match (Ranged Expr) [(Ranged Pat, Ranged Expr)]
    | Constant (Ranged Int)
    | Ivocation (Ranged Expr) (Ranged Expr)
    | Binding (Ranged String)
    | BinOp (Ranged BinOperator) (Ranged Expr) (Ranged Expr)

data Pat = PatInt (Ranged Int) | Any

data BinOperator = Sub | Mul | Dollar
