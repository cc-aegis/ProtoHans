module Eval (eval, invokeMain) where

import Data.Map ((!))
import Ranged
import Syntax

invokeMain :: Expr
invokeMain = ExInvocation (pure $ ExBinding $ pure "main") (pure ExWorldToken)

eval :: Definitions -> Expr -> Expr
eval _ lambda@(ExLambda _ _) = lambda
eval defs (ExMatch value cases) = eval defs $ getContent $ evalMatch defs (eval defs <$> value) cases
eval _ const@(ExConstant _) = const
eval defs (ExInvocation lambda value) = eval defs $ getContent $ evalInvocation defs (eval defs <$> lambda) value
eval defs (ExBinding (Ranged _ _ name)) = case defs ! name of Definition _ _ expr -> getContent expr
eval _ (ExBinOp _ _ _) = error "bin op unimplemented"
eval _ ExWorldToken = ExWorldToken

evalMatch :: Definitions -> Ranged Expr -> [(Ranged Pat, Ranged Expr)] -> Ranged Expr
evalMatch _ _ [] = error "no matching match case"
evalMatch _ _ ((Ranged _ _ PatAny, expr) : _) = expr
evalMatch defs value@(Ranged _ _ (ExConstant (Ranged _ _ c))) ((Ranged _ _ (PatConstant (Ranged _ _ c')), expr) : cases)
    | c == c' = expr
    | otherwise = evalMatch defs value cases
evalMatch defs value (_ : cases) = evalMatch defs value cases

evalInvocation :: Definitions -> Ranged Expr -> Ranged Expr -> Ranged Expr
evalInvocation defs (Ranged start end (ExLambda binding expr)) value = replaceBinding (binding, value) <$> expr
evalInvocation _ _ _ = error "cannot invoke"

replaceBinding :: (Ranged String, Ranged Expr) -> Expr -> Expr
replaceBinding subst@(Ranged _ _ binding, Ranged _ _ value) (ExLambda binding' body)
    | binding == getContent binding' = ExLambda binding' body
    | otherwise = ExLambda binding' $ replaceBinding subst <$> body
replaceBinding subst@(Ranged _ _ binding, Ranged _ _ value) (ExMatch matchValue matchCases) =
    ExMatch (replaceBinding subst <$> matchValue) ((\(pat, expr) -> (pat, replaceBinding subst <$> expr)) <$> matchCases)
replaceBinding _ const@(ExConstant _) = const
replaceBinding subst (ExInvocation lhs rhs) = (ExInvocation (replaceBinding subst <$> lhs) (replaceBinding subst <$> rhs))
replaceBinding subst@(Ranged _ _ binding, Ranged _ _ value) (ExBinding binding')
    | binding == getContent binding' = value
    | otherwise = ExBinding binding'
replaceBinding _ (ExBinOp _ _ _) = error "not yet implemented"
replaceBinding _ ExWorldToken = ExWorldToken
