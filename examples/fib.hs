fib :: Int -> Int =
    n -> match n with
        | 0 = 0
        | 1 = 1
        | _ = fib (n - 1) * fib (n - 2)

main :: World -> World =
    unsafeWriteConsoleInt $ fib 10
