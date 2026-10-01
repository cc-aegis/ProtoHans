module Result (CompilerError, Result) where

data Result a = Ok a | Err CompilerError

data CompilerError = Eof
    | UnexpectedChar Int Char

instance Functor Result where
    fmap f (Ok a) = Ok $ f a
    fmap _ (Err e) = Err e

instance Applicative Result where
    pure = Ok
    (Ok f) <*> (Ok x) = Ok $ f x
    (Err e) <*> _ = Err e
    _ <*> (Err e) = Err e

instance Monad Result where
    (Err e) >>= _ = Err e
    (Ok a) >>= f = f a
