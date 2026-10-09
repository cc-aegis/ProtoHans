# Expr
## ExLambda (implemented)
```
<ident> -> <expr>
```
## ExMatch
```
match <expr> with [| <pat> = <expr>]+
```
## ExInvocation (implemented)
```
<expr> <tinyexpr>
```
## ExBinOp
```
<expr> <binop> <expr>
```
## Other (implemented)
```
<tinyexpr>
```
# TinyExpr
## Brackets (implemented)
```
(<expr>)
```
## ExBinding (implemented)
```
<ident>
```
## ExConstant (implemented)
```
<number>
```
# Pat
## PatConstant
```
<number>
```
## PatAny
```
_
```
