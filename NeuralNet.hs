module NeuralNet where

import Debug.Trace

import GeneralDiff

data Layer a = Perceptron Int Int [[a]] | Activation (a -> a)

chunks n xs = pref : chunks n suff where (pref, suff) = splitAt n xs

layerParameterCount :: Layer a -> Int
layerParameterCount (Perceptron n m _) = (n+1) * m
layerParameterCount (Activation _) = 0

parameterCount :: [Layer a] -> Int
parameterCount ls = sum $ map layerParameterCount ls

layerParameters :: Layer a -> [a]
layerParameters (Perceptron _ _ ps) = concat ps
layerParameters (Activation _) = []

layerWithParameters :: Layer a -> [a] -> (Layer a, [a])
layerWithParameters (Perceptron n m _) ps = (Perceptron n m (take m $ chunks (n+1) ps), drop ((n+1)*m) ps)
layerWithParameters l@(Activation _) ps = (l, ps)

predict :: Num a => [Layer a] -> [a] -> [a]
predict [] xs = xs
predict (l:ls) xs = let n = length xs in case l of
  Perceptron n m ps -> predict ls (map (sum . zipWith (*) xs') ps) where xs' = 1:xs
  Activation f -> predict ls (map f xs)

networkParameters :: [Layer a] -> [a]
networkParameters = concatMap layerParameters
networkWithParameters :: [Layer a] -> [a] -> [Layer a]
networkWithParameters [] _ = []
networkWithParameters (l:ls) ps = l' : networkWithParameters ls ps' where (l', ps') = layerWithParameters l ps

networkGradientStep :: Num a => (([B a] -> [B a]) -> B a) -> a -> [Layer (B a)] -> [Layer (B a)]
networkGradientStep loss lr network =
  let
    ps = map valB $ networkParameters network
    delta = gradient (loss . predict . networkWithParameters network) ps
    ps' = map constB $ zipWith (+) ps $ map (* (-lr)) delta
  in networkWithParameters network ps'

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