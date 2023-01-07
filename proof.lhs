> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}

> data Phi a = Phi a a deriving (Show, Eq)

> class Lift a b where
>   ι :: a -> b a

Given a ring R this defines R[ϕ]/(ϕ²-ϕ-1)

> instance Num a => Num (Phi a) where
>     fromInteger a = Phi (fromInteger a) 0
>     Phi a b  + Phi a' b' = Phi (a + a') (b + b')
>     Phi a b * Phi a' b' = Phi (a * a' + b * b') (a * b' + a' * b + b * b')
>     negate (Phi a b) = Phi (negate a) (negate b)

> instance Fractional a => Fractional (Phi a) where
>     recip (Phi a b) =
>         let denominator = a * a + a * b - b * b
>         in Phi ((a + b)/denominator) ( - b/denominator)

> instance Num a => Lift a Phi where
>   ι a = Phi a 0

Given a ring R this defines R[x]/(x²-1)

> data Idem a = Idem a a deriving (Show, Eq)

> instance Num a => Num (Idem a) where
>     fromInteger a = Idem (fromInteger a) 0
>     Idem a b  + Idem a' b' = Idem (a + a') (b + b')
>     Idem a b * Idem a' b' = Idem (a * a' + b * b') (a * b' + a' * b)
>     negate (Idem a b) = Idem (negate a) (negate b)

> instance Num a => Lift a Idem where
>   ι a = Idem a 0

> data Laurent a = Laurent Int [a] deriving Show

> decanonical :: Num a => Int -> Laurent a -> [a]
> decanonical m (Laurent n as) | m < n = replicate (n - m) 0 ++ as
>                              | True = as

> (===) :: (Eq a, Num a) => [a] -> [a] -> Bool
> [] === [] = True
> [] === (b : bs) = b == 0 && [] === bs
> (a : as) === [] = a == 0 &&  as === []
> (a : as) === (b : bs) = a == b && as === bs

Given a ring R this defines the ring of Laurent polynomials R[[x, x⁻¹]]

> instance (Eq a, Num a) => Eq (Laurent a) where
>   Laurent m as == Laurent n bs =
>     let mn = min m n
>     in decanonical mn (Laurent m as) === decanonical mn (Laurent n bs)

> plus :: Num a => [a] -> [a] -> [a]
> plus [] as = as
> plus as [] = as
> plus (a : as) (a' : as') =
>   let bs = plus as as'
>   in (a + a') : bs

> times :: Num a => [a] -> [a] -> [a]
> times [] _ = []
> times (a : as) bs =
>   let cs = times as bs
>   in plus (map (a *) bs) (0 : cs)

> instance Num a => Num (Laurent a) where
>     fromInteger a = Laurent 0 [fromInteger a]
>     Laurent m as + Laurent n bs =
>       let mn = min m n
>           as' = decanonical mn (Laurent m as)
>           bs' = decanonical mn (Laurent n bs)
>       in Laurent mn (plus as' bs')
>     Laurent m as * Laurent n bs =
>       Laurent (m + n) (times as bs)
>     negate (Laurent m as) = Laurent m $ map negate as

> instance Lift a Laurent where
>   ι a = Laurent 0 [a]

> ϕ :: Phi Rational
> ϕ = Phi 0 1

> type FExp = Laurent (Laurent (Idem (Idem (Phi Rational))))
> α, α', αᵐ, αⁿ, β, β', βᵐ, βⁿ, negateᵐ, negateⁿ, invsqrt5 :: FExp

> ι4 = ι . ι . ι . ι
> α = ι4 (Phi 0 1)
> β = 1 - α
> α' = -β
> β' = -α
> αᵐ = Laurent 1 [1]
> αᵐ' = Laurent (-1) [1]
> βᵐ = negateᵐ * αᵐ'
> βᵐ' = negateᵐ * αᵐ
> αⁿ = ι (Laurent 1 [1])
> αⁿ' = ι (Laurent (-1) [1])
> βⁿ = negateⁿ * αⁿ'
> βⁿ' = negateⁿ * αⁿ
> negateᵐ = ι (ι (Idem 0 1))
> negateⁿ = ι (ι (ι (Idem 0 1)))
> invsqrt5 = ι4 ((2 * ϕ - 1) / 5)

This allows negative powers without using division.
Useful if you want to perform a computation in a ring that
just so happens to also be a field even though you don't want to
assume the field structure.
To compute xⁿ you provide both x and x⁻¹ as argments.

> pow :: Num a => a -> a -> Int -> a
> pow x x' n | n == 0 = 1
>            | n > 0 = x * pow x x' (n - 1)
>            | n < 0 = x' * pow x x' (n + 1)

Direct application of Binet formula for F(am+bn+c)

> fib :: Int -> Int -> Int -> FExp
> fib a b c = (pow α α' c * pow αᵐ αᵐ' a * pow αⁿ αⁿ' b
>                - pow β β' c * pow βᵐ βᵐ' a * pow βⁿ βⁿ' b) * invsqrt5

> luc :: Int -> Int -> Int -> FExp
> luc a b c = pow α α' c * pow αᵐ αᵐ' a * pow αⁿ αⁿ' b
>                + pow β β' c * pow βᵐ βᵐ' a * pow βⁿ βⁿ' b

Lots of examples at
http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibFormulae.html

> main = do
>   putStrLn "Following should be true"

    F(m+2) = F(m) + F(m+1)

>   print $ fib 1 0 2 == fib 1 0 0 + fib 1 0 1

    F(n+2) = F(n) + F(n+1)

>   print $ fib 0 1 2 == fib 0 1 0 + fib 0 1 1

    F(n)F(n+1) = F(n-1)F(n+2) + (-1)ⁿ⁻¹

>   print $ fib 1 0 0 * fib 1 0 1 == fib 1 0 (-1) * fib 1 0 2 - negateᵐ

    F(n) = F(m)F(n+1-m) + F(m-1)F(n-m)

>   print $ fib 0 1 0 == fib 1 0 0 * fib (-1) 1 1 + fib 1 0 (-1) * fib (-1) 1 0

    F(n)²F(m + 1)F(m - 1) - F(m)²F(n + 1)F(n - 1) + (-1)ⁿF(m + n)F(m - n) = 0

>   print $ fib 0 1 0 ^ 2 * fib 1 0 1 * fib 1 0 (-1)
>          - fib 1 0 0 ^ 2 * fib 0 1 1 * fib 0 1 (-1)
>          + negateⁿ * fib 1 1 0 * fib 1 (-1) 0 == 0

    F(n+k)² + F(n−k)² = F(n+k-2)F(n+k+1) + F(2k-1)F(2n-1)

>   print $ fib 1 1 0 ^ 2 + fib (-1) 1 0 ^ 2
>           == fib 1 1 (-2) * fib 1 1 1 + fib 2 0 (-1) * fib 0 2 (-1)

    F(n+3)² + F(n)² = 2(F(n+1)² + F(n+2)²)

>   print $ fib 1 0 3 ^ 2 + fib 1 0 0 ^ 2 == 2 * (fib 1 0 1 ^ 2 + fib 1 0 2 ^ 2)

    L(n+m) + (−1)ᵐL(n-m) = L(m)L(n) 

>   print $ luc 1 1 0 + negateᵐ * luc (-1) 1 0 == luc 1 0 0 * luc 0 1 0

    5F(m)F(n) = L(n + m) - (-1)ᵐL(n-m)

>   print $ 5 * fib 1 0 0 * fib 0 1 0 == luc 1 1 0 - negateᵐ * luc (-1) 1 0

>   putStrLn "Following should be false"

    Some falsities

>   print $ fib 0 1 2 == fib 1 1 0 + fib 0 1 1
>   print $ 5 * fib 1 0 0 * fib 0 1 0 == luc 1 1 0 - negateⁿ * luc (-1) 1 0
>   print $ fib 1 0 3 ^ 2 + fib 1 0 0 ^ 2 == 2 * (fib 1 0 1 ^ 2 - fib 1 0 2 ^ 2)
>   print $ fib 0 1 0 == fib 1 0 0 * fib 1 1 1 + fib 1 0 1 * fib 1 1 0
