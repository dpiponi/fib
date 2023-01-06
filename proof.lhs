> {-# OPTIONS_GHC -fno-warn-missing-methods #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

> import qualified Data.Map.Strict as M
> import Data.Ratio
> import Data.Monoid
> import Data.Group
> import Control.Applicative

> fibonacci n = (α^^n - β^^n)/(α - β)
>               where α = ϕ
>                     β =  - 1/ϕ

> main = do
>     print $ map fibonacci [0..10]

Q is the field ℚ[ϕ]/(ϕ²-ϕ-1)

> data Q = Q Rational Rational deriving Eq

> instance Show Q where
>     show (Q a b) = show a ++ (if b == 0 then "" else " + " ++ show b ++ "ϕ")

> instance Num Q where
>     fromInteger a = Q (fromInteger a) 0
>     Q a b  + Q a' b' = Q (a + a') (b + b')
>     Q a b * Q a' b' = Q (a * a' + b * b') (a * b' + a' * b + b * b')
>     negate (Q a b) = Q (negate a) (negate b)

> instance Fractional Q where
>     recip (Q a b) =
>         let denominator = a * a + a * b - b * b
>         in Q ((a + b)/denominator) ( - b/denominator)

> ϕ = Q 0 1
> α = ϕ
> β = 1 - α

> sqrt5 = 2 * ϕ - 1

Integers modulo 2

This is wrong. Need canonical form for reduce to work.

> type Z2 = Bool

> instance Semigroup Bool where
>   (<>) = (/=)

> instance Monoid Bool where
>   mempty = False

> instance Group Bool where
>   invert = id

> instance Semigroup Int where
>     (<>) = ( + )

> instance Monoid Int where
>     mempty = 0

> instance Group Int where
>     invert = negate

> instance Abelian Int

> type Expr a b c = GroupRing a (b, c)

> 𝚤 :: Monoid g => r -> GroupRing r g
> 𝚤 r = GR [(mempty, r)]

> 𝚥 :: Num r => g -> GroupRing r g
> 𝚥 g = GR [(g, 1)]

> lift :: Num r => (g -> r) -> GroupRing r g -> r
> lift f (GR gs) = sum [r * f g | (g, r) <- gs]

> newtype GroupRing r g = GR [(g, r)] deriving Show

> reduce :: (Num r, Eq r, Ord g) => [(g, r)] -> [(g, r)]
> reduce = filter ((/= 0) . snd) . M.toList . M.fromListWith ( + )

> instance (Num r, Eq r, Group g, Ord g) => Num (GroupRing r g) where
>     fromInteger a = GR $ reduce $ [(mempty, fromInteger a)]
>     GR as + GR bs = GR $ reduce $ as ++ bs
>     GR as * GR bs = GR $ reduce $ m <$> as <*> bs
>                     where m (g, r) (g', r') = (g <> g', r * r')
>     negate (GR as) = GR $ fmap (fmap negate) as

Now we extend the ring Q by adding in a new symbol ϕⁿ.
(Even though that looks like an expression, we treat it as a monolithic symbol.)
nⁿ represents (-1)ⁿ

> ϕⁿ, αⁿ, βⁿ, nⁿ :: (Z2, Int)
> ϕⁿ = (False, 1)
> αⁿ = (False, 1)
> βⁿ = (True, -1)
> nⁿ = (True, 0)

Expression for F(n)

> fib :: GroupRing Q (Z2, Int)
> fib = (𝚥 ϕⁿ  - 𝚥 βⁿ) * 𝚤 (1/sqrt5)

Expression for Fibonacci number F(an + b)

> fib' a b = (𝚤 (α^^b) * (𝚥 $ ϕⁿ `pow` a) - 𝚤 (β^^b) * (𝚥 $ βⁿ `pow` a)) * 𝚤 (1/sqrt5)
> lucas' a b = (𝚤 (α^^b) * (𝚥 $ ϕⁿ `pow` a) + 𝚤 (β^^b) * (𝚥 $ βⁿ `pow` a))

> f 0 = 0
> f 1 = 1
> f n = f (n -1) + f (n - 2)

> ex1 :: GroupRing Q (Z2, Int)
> ex1 = fib' 1 (-1)^2 * fib' 1 1^2 - fib' 1 (-2)^2 * fib' 1 2^2 - 4 * 𝚥 nⁿ * fib' 1 0^2

> newtype V a = V [a] deriving (Eq, Show, Ord, Functor)

> instance (Eq a, Monoid a) => Semigroup (V a) where
>     (<>) (V a) (V b) = V $ trim (a `vmappend` b) where
>       vmappend (a:as) (b:bs) = (a <> b : vmappend as bs)
>       vmappend a [] = a
>       vmappend [] b = b
>       trim as | all (== mempty) as = []
>       trim (a : as) = a : trim as

> instance (Eq a, Monoid a) => Monoid (V a) where
>     mempty = V []

> instance (Eq a, Group a) => Group (V a) where
>     invert (V as) = V $ map invert as

> instance (Eq a, Abelian a) => Abelian (V a) 

> (!) :: Monoid a => V a -> Int -> a
> V [] ! _ = mempty
> V (a : _) ! 0 = a
> V (a : as) ! i = V as ! (i - 1)

Similar but we extend with two new symbols: ϕⁱ and ϕ^j

> ϕⁱ, αⁱ, βⁱ, ni :: (V Z2, V Int)
> ϕⁱ = (V [False], V [1])
> αⁱ = (V [False], V [1])
> βⁱ = (V [True], V [-1])


ni represents (-1)ⁱ

> ni = (V [True], V [])

> ϕj, αʲ, βʲ, nʲ :: (V Z2, V Int)
> ϕj = (V [False, False], V [0, 1])
> αʲ = (V [False, False], V [0, 1])
> βʲ = (V [False, True], V [0, -1])
> nʲ = (V [False, True], V [])

> αpow :: Int -> Int -> Expr Q (V Z2) (V Int)
> αpow a b = 𝚥 $ (αⁱ `pow` a) <> (αʲ `pow` b)
> βpow :: Int -> Int -> Expr Q (V Z2) (V Int)
> βpow a b = 𝚥 $ (βⁱ `pow` a) <> (βʲ `pow` b)

Expression for Fibonacci number F(a * i  + b * j  + c)

> fib'' a b c = (𝚤 (α^^c) * αpow a b  - 𝚤 (β^^c) * βpow a b) * 𝚤 (1/sqrt5)
> lucas'' a b c = (𝚤 (α^^c) * αpow a b  + 𝚤 (β^^c) * βpow a b)

> dot :: Num a => V a -> V a -> a
> dot (V a) (V b) = sum $ zipWith (*) a b

> unBool False = 0
> unBool True = 1

> evalTerm' :: Int -> Int -> (V Z2, V Int) -> Q
> evalTerm' i j (b, c) = (-1) ^^ dot ij (fmap unBool b)  * 
>                        ϕ    ^^ dot ij c
>   where ij = V [i, j]

> evalExpr' :: Int -> Int -> Expr Q (V Z2) (V Int) -> Q
> evalExpr' i j = lift (evalTerm' i j)

> ex2 = fib'' 1 1 0 + 𝚥 nʲ * fib'' 1 (-1) 0  - fib'' 1 0 0 * lucas'' 0 1 0
> ex3 = 2 * fib'' 1 1 0 - lucas'' 1 0 0 * fib'' 0 1 0 - lucas'' 0 1 0 * fib'' 1 0 0

F(n)²F(m + 1)F(m - 1)  - F(m)²F(n + 1)F(n - 1)  + (-1)ⁿF(m + n)F(m - n) = 0

Reminder: fib'' a b c = F(a * i  + b * j  + c) for unknowns i and j

> ex4 = fib'' 0 1 0 ^ 2 * fib'' 1 0 1 * fib'' 1 0 (-1)
>     - fib'' 1 0 0 ^ 2 * fib'' 0 1 1 * fib'' 0 1 (-1)
>     + 𝚥 nʲ * fib'' 1 1 0 * fib'' 1 (-1) 0

Eg. see http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibFormulae.html
