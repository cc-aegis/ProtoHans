fib :: Int -> Int =
    n -> match n
        | 0 = 0
        | 1 = 1
        | _ = mul (fib (sub n 1)) (fib (sub n 2))

main :: World -> World =
    unsafeWriteConsoleInt (fib 10)
