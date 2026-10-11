cons :: a -> List a -> List a =
    x -> xs -> q -> match q with
        | 0 = x
        | 1 = xs

nil :: List a = 0

head :: List a -> a = list -> list 0
tail :: List a -> List a = list -> list 1

nums :: List Int = cons 0 (cons 1 (cons 2 (cons 3 nil)))
rec :: List Int = cons 1 rec

nth :: Int -> List a -> a =
    idx -> match idx with
        | 0 = head
        | _ = dot (nth (dec idx)) tail

dot :: (b -> c) -> (a -> b) -> a -> c =
    f -> g -> x -> f (g x)

add :: Int -> Int -> Int =
    x -> y -> match y with
        | 0 = x
        | _ = add (inc x) (dec y)

zipWith :: (a -> b -> c) -> List a -> List b -> List c =
    f -> lhs -> rhs -> cons (f (head lhs) (head rhs)) (zipWith f (tail lhs) (tail rhs))

fibs :: List Int = cons 0 (cons 1 (zipWith add fibs (tail fibs)))
