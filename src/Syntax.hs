module Syntax (Definitions, Definition(..), Format(..), Type(..), Expr(..), Pat(..), BinOperator(..)) where

import Ranged (Ranged(..))

import Control.Comonad (Comonad(..))
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
    | ExWorldToken
    deriving (Show)

data Pat = PatConstant (Ranged Int) | PatAny
    deriving (Show)

data BinOperator = Sub | Mul | Dollar
    deriving (Show)

class Format f where
    format :: f -> String

instance Format Definition where
    format (Definition name type' expr) = show (extract name) ++ " :: " ++ format (extract type') ++ " = " ++ format (extract expr)

instance Format Type where
    format TyInt = "Int"
    format TyWorld = "World"
    format (TyFunction lhs@(Ranged _ _ (TyFunction _ _)) rhs) = "(" ++ format (extract lhs) ++ ")" ++ " -> " ++ format (extract rhs)
    format (TyFunction lhs rhs) = format (extract lhs) ++ " -> " ++ format (extract rhs)

instance Format Expr where
    format (ExLambda binding expr) = extract binding ++ " -> " ++ format (extract expr)
    format (ExMatch value cases) = "match " ++ format (extract value) ++ " with" ++ concatMap (\ (pat, expr) -> " | " ++ format (extract pat) ++ " = " ++ format (extract expr)) cases
    format (ExConstant const) = show (extract const)
    format (ExInvocation lhs rhs@(Ranged _ _ (ExConstant _))) = format (extract lhs) ++ " " ++ format (extract rhs)
    format (ExInvocation lhs rhs@(Ranged _ _ (ExBinding _))) = format (extract lhs) ++ " " ++ format (extract rhs)
    format (ExInvocation lhs rhs) = format (extract lhs) ++ " (" ++ format (extract rhs) ++ ")"
    format (ExBinding binding) = extract binding
    format (ExBinOp op lhs rhs) = "(" ++ show (extract lhs) ++ format (extract op) ++ show (extract rhs) ++ ")"
    format ExWorldToken = "※"

instance Format Pat where
    format (PatConstant const) = show (extract const)
    format (PatAny) = "_"

instance Format BinOperator where
    format Sub = "-"
    format Mul = "+"
    format Dollar = "$"
