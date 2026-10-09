# Expr
## ExLambda
```
<ident> -> <expr>
```
## ExMatch
```
match <expr> with [| <pat> = <expr>]+
```
## ExInvocation
```
<expr> <tinyexpr>
```
## ExBinOp
```
<expr> <binop> <expr>
```
## Other
```
<tinyexpr>
```
# TinyExpr
## Brackets
```
(<expr>)
```
## ExBinding
```
<ident>
```
## ExConstant
```
<number>
```
