answer :: Int = 42

id :: Int -> Int = x -> x

fib :: Int -> Int =
    n -> match n with
        | 0 = 0
        | 1 = 1
        | _ = add (fib (sub n 1)) (fib (sub n 2))

sub :: Int -> Int -> Int =
    x -> y -> match y with
        | 0 = x
        | _ = sub (dec x) (dec y)

add :: Int -> Int -> Int =
    x -> y -> match y with
        | 0 = x
        | _ = add (inc x) (dec y)

mul :: Int -> Int -> Int =
    x -> y -> match y with
        | 0 = 0
        | _ = (add x (mul x (dec y)))

sqr :: Int -> Int = x -> mul x x
