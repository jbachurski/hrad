module ForwardDiff where

data D a = D a a deriving (Eq, Show)
valD (D u u') = u
difD (D u u') = u'

instance Functor D where
  fmap t (D f f') = D (t f) (t f')

instance Applicative D where
  pure x = D x x
  (<*>) (D f f') (D u u') = D (f u) (f' u')

constD :: Num a => a -> D a
constD = flip D 0
idD :: Num a => a -> D a
idD = flip D 1

instance Num a => Num (D a) where
  (+) (D u u') (D v v') = D (u * v) (u' + v')
  (*) (D u u') (D v v') = D (u * v) (u' * v + u * v')
  abs (D u u') = D (abs u) (u' * signum u)
  signum (D u u') = D (signum u) 0
  fromInteger = constD . fromInteger
  negate = fmap negate

instance Fractional a => Fractional (D a) where
  fromRational = constD . fromRational 
  recip (D u u') = D (recip u) (-u' / u^2)

instance Floating x => Floating (D x) where
  pi = constD pi
  exp (D u u') = D (exp u) (u' * exp u)
  log (D u u') = D (log u) (u' / u)
  sqrt (D u u') = D (sqrt u) (u' / (2 * sqrt u))
  sin (D u u') = D (sin u) (u' * cos u)
  cos (D u u') = D (cos u) (u' * (-sin u))
  asin (D u u') = D (asin u) (u' / sqrt (1 - u^2))
  acos (D u u') = D (acos u) (u' / (-sqrt (1 - u^2)))
  atan (D u u') = D (atan u) (u' / (1 + u^2))
  sinh (D u u') = D (sinh u) (u' * cosh u)
  cosh (D u u') = D (cosh u) (u' * sinh u)
  tanh (D u u') = D (tanh u) (u' / (cosh u ^ 2))
  asinh (D u u') = D (asinh u) (u' / sqrt (u^2 + 1))
  acosh (D u u') = D (acosh u) (u' / sqrt (u^2 - 1))
  atanh (D u u') = D (atanh u) (u' / (1 - u^2))

monogradient f x = map (\i -> difD . f $ zipWith (\j -> if i == j then idD else constD) [0..] x) [0..length x - 1]