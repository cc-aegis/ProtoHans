# ProtoHans
This is a prototype for a Haskell-inspired functional programming language called Hans.

# How to Use
Run this command to run ProtoHans with a specific file (like ghci):
```sh
cabal run ProtoHans -- .\examples\math.hs
```
After that, you can repeatedly type queries to run (or `exit` to exit). Example:
```
query: fib 10
55
query: id
x -> x
query: id id id miku
39
query: add (fib 4) (sqr (dec 3))
7
```

Please note: `math.hs` is the only example that currently works :/
