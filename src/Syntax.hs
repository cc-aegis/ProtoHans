module Syntax (Definitions, Definition(..), Type(..), Expr(..), Pat(..), BinOperator(..)) where

import Ranged (Ranged)

import Data.Map (Map)

type Definitions = Map String Definition

data Definition = Definition (Ranged String) (Ranged Type) (Ranged Expr)
    deriving (Show)

data Type = TyInt | TyWorld | TyFunction (Ranged Type) (Ranged Type)
    deriving (Show)

data Expr = ExLambda (Ranged String) (Ranged Expr)
    | ExMatch (Ranged Expr) [(Ranged Pat, Ranged Expr)]
    | ExConstant (Ranged Int)
    | ExInvocation (Ranged Expr) (Ranged Expr)
    | ExBinding (Ranged String)
    | ExBinOp (Ranged BinOperator) (Ranged Expr) (Ranged Expr)
    deriving (Show)

data Pat = PatInt (Ranged Int) | Any
    deriving (Show)

data BinOperator = Sub | Mul | Dollar
    deriving (Show)
