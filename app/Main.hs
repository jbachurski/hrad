module Main where

import System.Random
import Data.List
import Control.Monad

import GeneralDiff
import NeuralNet

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

relu :: Fractional a => a -> a
relu x = (abs x + x) / 2

networkArch :: [Layer (B Double)]
networkArch = [Perceptron 1 3 [], Activation sigmoid, Perceptron 3 3 [], Activation sigmoid, Perceptron 3 1 []]

initWeights n = replicateM n (randomRIO (-2,2::Double))

xsExample = map singleton [-3,-2.5..3] :: [[Double]]
ysExample = map cos <$> xsExample
loss = mse (map constB <$> xsExample) (map constB <$> ysExample)

reps :: (Ord a, Num a) => (t -> t) -> t -> [a] -> [t]
reps f =
  let
    go i x [] = []
    go i x js@(j:js')
      | i == j = x : go i (f x) js'
      | i > j = go i x js'
      | otherwise = go (i+1) (f x) js
  in
    go 0

main = do
  setStdGen $ mkStdGen 1337
  paramsIni <- initWeights (parameterCount networkArch)
  print "Hello!"
  let
    networkIni = networkWithParameters networkArch (map constB paramsIni)
    stops = [0, 1, 2, 5, 10, 20, 50, 100, 200, 1000, 5000, 20000, 50000]
    training = zip stops $ reps (networkGradientStep loss 5e-3) networkIni stops
    final = snd $ last training
    desc i =
      let
        (j, network) = training !! i
        l = valB $ loss (predict network)
      in putStrLn $ "Epoch " ++ show j ++ ": " ++ show l -- ++ ", " ++ show ps ++ "; " ++ show (gradientB (loss . predict network) ps)
  print $ loss (sin <$>)
  print $ length training
  mapM_ desc [0..length stops - 1]
  print ysExample
  print $ map valB <$> map (predict final) (map constB <$> xsExample)