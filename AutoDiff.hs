{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module GeneralDiff where

import Data.Array

class AD a t where
  (|+|) :: a t -> a t -> a t
  (|*|) :: a t -> a t -> a t
  (|><|) :: (t -> t) -> (t -> t) -> a t -> a t
  constAD :: t -> a t
  
instance (AD a t, Num t) => Num (a t) where
  fromInteger = constAD . fromInteger
  (+) = (|+|)
  (*) = (|*|)
  abs = abs |><| signum
  signum = signum |><| const 0
  negate = negate |><| const (-1)

instance (AD a t, Fractional t) => Fractional (a t) where
  fromRational = constAD . fromRational
  recip = recip |><| (\x -> -1 / (x^2))

instance (AD a t, Floating t) => Floating (a t) where
  pi = constAD pi
  exp = exp |><| exp
  log = log |><| recip
  sqrt = sqrt |><| (\x -> 1 / (2 * sqrt x))
  sin = sin |><| cos
  cos = cos |><| (negate . sin)
  asin = asin |><| (\x ->  1 / sqrt (1 - x^2))
  acos = acos |><| (\x -> -1 / sqrt (1 - x^2))
  atan = atan |><| (\x ->  1 / (1 + x^2))
  sinh = sinh |><| cosh
  cosh = cosh |><| sinh
  tanh = tanh |><| (\x -> cosh x ^^ (-2))
  asinh = asinh |><| (\x ->  1 / sqrt (x^2 + 1))
  acosh = acosh |><| (\x -> -1 / sqrt (x^2 - 1))
  atanh = atanh |><| (\x ->  1 / (1 - x^2))


data B a = B a (a -> [(Int, a)] -> [(Int, a)])

instance (Num a, Show a) => Show (B a) where
  show (B u go) = "B " ++ show u ++ " ε" ++ show (atSum $ go 1 [])

valB :: B a -> a
valB (B u _) = u
goB :: B a -> a -> [(Int, a)] -> [(Int, a)]
goB (B _ go) = go

constB :: a -> B a
constB c = B c (\_ ds -> ds)
idB :: Int -> b -> B b
idB i xi = B xi (\d ds -> (i, d) : ds)

instance Num t => AD B t where
  (|+|) (B u goU) (B v goV) = B (u + v) (\d ds -> goU d (goV d ds))
  (|*|) (B u goU) (B v goV) = B (u * v) (\d ds -> goU (d * v) (goV (d * u) ds))
  (|><|) f f' (B u goU) = B (f u) (\d -> goU (d * f' u))
  constAD = constB

atSum :: (Ix i, Num e, Num i) => [(i, e)] -> [e]
atSum ps = elems $ accumArray (+) 0 (0, n) ps where n = if null ps then -1 else maximum (map fst ps)

gradientB :: ([B a] -> t) -> [a] -> t
gradientB f x = f (zipWith idB [0..] x)
gradientContributions :: Num a => ([B b] -> B a) -> [b] -> [(Int, a)]
gradientContributions f x = goB (gradientB f x) 1 []
gradient :: Num a => ([B b] -> B a) -> [b] -> [a]
gradient f x = atSum $ gradientContributions f x

data D a = D a a deriving (Eq, Show)
valD :: D a -> a
valD (D u u') = u
difD :: D a -> a
difD (D u u') = u'

constD :: Num a => a -> D a
constD c = D c 0
idD :: Num a => a -> D a
idD x = D x 1

instance Num t => AD D t where
  (|+|) (D u u') (D v v') = D (u + v) (u' + v')
  (|*|) (D u u') (D v v') = D (u * v) (u * v' + u' * v)
  (|><|) f f' (D u u') = D (f u) (u' * f' u)
  constAD = constD

monogradient :: Num a => ([D a] -> D b) -> [a] -> [b]
monogradient f x = map (\i -> difD . f $ zipWith (\j -> if i == j then idD else constD) [0..] x) [0..length x - 1]


data GradientDescent b a = GD a (a -> b -> a)

descending c = GD c (\δ x -> c - δ)

instance Num a => AD (GradientDescent b) a where
  (|+|) (GD u descU) (GD v descV) = GD (u + v) (\δ x -> descU δ x + descV δ x)
  (|*|) (GD u descU) (GD v descV) = GD (u * v) (\δ x -> descU (v * δ) x * v + descV (u * δ) x)
  (|><|) f f' (GD u desc) = GD (f u) (\δ x -> desc (f' u * δ) x)
  constAD c = GD c (\δ x -> c)
