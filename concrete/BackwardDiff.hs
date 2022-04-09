module BackwardDiff where

import Data.List
import Debug.Trace

data B a = B a (a -> [(Int, a)] -> [(Int, a)])

instance (Num a, Show a) => Show (B a) where
  show (B u go) = "B " ++ show u ++ " λ" ++ show (indexSums $ go 1 [])

constB c = B c (\_ ds -> ds)

valB (B u _) = u

(><) f f' (B u goU) = B (f u) (\d -> goU (d * f' u))

instance (Show a, Num a) => Num (B a) where
  fromInteger = constB . fromInteger
  (+) (B u goU) (B v goV) = B (u + v) (\d ds -> goU d (goV d ds))
  (*) (B u goU) (B v goV) = B (u * v) (\d ds -> goU (d * v) (goV (d * u) ds))
  abs = abs >< signum
  signum = signum >< const 0
  negate = negate >< const (-1)

instance (Show a, Fractional a) => Fractional (B a) where
  fromRational = constB . fromRational
  recip = recip >< (\x -> -1 / (x^2))

instance (Show a, Floating a) => Floating (B a) where
  pi = constB pi
  exp = exp >< exp
  log = log >< recip
  sqrt = sqrt >< (\x -> 1 / (2 * sqrt x))
  sin = sin >< cos
  cos = cos >< (negate . sin)
  asin = asin >< (\x ->  1 / sqrt (1 - x^2))
  acos = acos >< (\x -> -1 / sqrt (1 - x^2))
  atan = atan >< (\x -> 1 / (1 + x^2))
  sinh = sinh >< cosh
  cosh = cosh >< sinh
  tanh = tanh >< (\x -> cosh x ^^ (-2))
  asinh = asinh >< (\x ->  1 / sqrt (x^2 + 1))
  acosh = acosh >< (\x -> -1 / sqrt (x^2 - 1))
  atanh = atanh >< (\x -> 1 / (1 - x^2))

indexSums :: Num a => [(Int, a)] -> [a]
indexSums vs = 
  let
    n = if null vs then 0 else maximum (map fst vs)
    vs' = sortBy (\(i, xi) (j, xj) -> compare i j) $ vs ++ zip [0..n] (replicate n 0)
  in
    map (sum . map snd) $ groupBy (\(i, xig) (j, xj) -> i == j) vs'

gradientB f x = f (zipWith (\i xi -> B xi (\d ds -> (i, d) : ds)) [0..] x)
gradientContr f x = case gradientB f x of B _ go -> go 1 []
gradient f x = indexSums (gradientContr f x)