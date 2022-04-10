module NeuralNet where

import GeneralDiff

data Layer a = Dim Int | Perceptron Int | Activation (a -> a)

parameterCount :: [Layer a] -> Int
parameterCount = 
  let
    go _ [] = 0
    go n (l:ls) = case l of
      Dim n' -> if n /= 0 && n /= n' then error "Dimension mismatch in network" else go n' ls
      Perceptron m -> if n == 0 then error "Unknown input shape" else (n+1)*m + go m ls
      Activation _ -> go n ls
  in go 0 

predict :: Num a => [Layer a] -> [a] -> [a] -> [a]
predict [] _ xs = xs
predict (l:ls) ps xs = let n = length xs in case l of
  Dim n' -> if n /= n' then error "Dimension mismatch in network" else predict ls ps xs
  Perceptron m ->
    let cnt = (n+1) * m
        dots ps = sum (zipWith (*) (take (n+1) ps) (1:xs)) : dots (drop (n+1) ps)
        ys = take m $ dots ps
        qs = drop cnt ps
    in if length ps < cnt then error "Missing parameters in network" else predict ls qs ys
  Activation f -> predict ls ps (map f xs)

gradientStep :: Num c => ([B c] -> b) -> (b -> B c) -> c -> [c] -> [c]
gradientStep paramPred loss lr ps =
  zipWith (+) ps $ map (* (-lr)) grad
  where grad = gradient (loss . paramPred) ps

epoch :: (Num c, Show c) => [Layer (B c)] -> (([B c] -> [B c]) -> B c) -> c -> [c] -> [c]
epoch layers = gradientStep $ predict layers
train :: (Num c, Show c) => [Layer (B c)] -> (([B c] -> [B c]) -> B c) -> c -> [c] -> [[c]]
train layers loss lr = iterate $ epoch layers loss lr

dimensionCheck :: (Foldable t1, Foldable t2) => [t1 a1] -> [t2 a2] -> a3 -> a3
dimensionCheck xs ys =
  if (length xs == length ys) && and (zipWith (==) (map length xs) (map length ys))
  then id else error "Dimension mismatch"

mse :: Fractional y => [x] -> [[y]] -> (x -> [y]) -> y
mse xs ys pred =
  let
    ys' = map pred xs
    l = sum $ zipWith (\x y -> (x - y)^2) (concat ys) (concat ys')
  in dimensionCheck ys ys' $ l / fromIntegral (length (concat ys))