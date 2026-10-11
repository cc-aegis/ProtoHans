cons :: a -> List a -> List a =
    x -> xs -> q -> match q with
        | 0 = x
        | 1 = xs

nil :: List a = 0

isNil :: List a -> (b -> b -> b) =
    list -> match list with
        | 0 = true
        | _ = false

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

y :: (a -> a) -> a = f -> (x -> f (x x)) (x -> f (x x))

zipWith :: (a -> b -> c) -> List a -> List b -> List c =
    f -> lhs -> rhs -> cons (f (head lhs) (head rhs)) (zipWith f (tail lhs) (tail rhs))

fibs :: List Int = y (self -> cons 0 (cons 1 (zipWith add self (tail self))))

true :: a -> a -> a = x -> y -> x
false :: a -> a -> a = x -> y -> y

filter :: (a -> Bool) -> List a -> List a =
    cond -> list -> (isNil list)
        nil
        (cond (head list)
            (cons (head list) (filter cond (tail list)))
            (filter cond (tail list)))

map :: (a -> b) -> List a -> List b =
    f -> list -> (isNil list)
        nil
        (cons (f (head list)) (map f (tail list)))

foldl :: (b -> a -> b) -> b -> List a -> b =
    join -> acc -> list -> (isNil list)
        acc
        (foldl join (join acc (head list)) (tail list))

take :: Int -> List a -> List a =
    n -> l -> match n with
        | 0 = nil
        | _ = cons (head l) (take (dec n) (tail l))

last :: List a -> a =
    l -> (isNil (tail l))
        (head l)
        (last (tail l))
