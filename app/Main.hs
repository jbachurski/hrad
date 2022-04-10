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

network :: [Layer (B Double)]
network = [Dim 1, Perceptron 3, Activation sigmoid, Perceptron 3, Activation sigmoid, Perceptron 1, Dim 1]

initWeights n = replicateM n (randomRIO (-1,1::Double))

xsExample :: [[B Double]]
xsExample = map (singleton . constB) [-3,-2.5..3]
ysExample :: [[B Double]]
ysExample = map (singleton . (\[x] -> cos x)) xsExample
loss :: ([B Double] -> [B Double]) -> B Double
loss = mse xsExample ysExample

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
  ps0 <- initWeights $ parameterCount network
  let
    stops = [0, 1, 2, 5, 10, 20, 50, 100, 200, 1000, 5000]
    netEpoch = epoch network loss 5e-3
    training = zip stops $ reps netEpoch ps0 stops
    desc i =
      let
        (j, ps) = training !! i
        l = valB $ loss (predict network (map constB ps))
      in putStrLn $ "Epoch " ++ show j ++ ": " ++ show l -- ++ ", " ++ show ps ++ "; " ++ show (gradientB (loss . predict network) ps)
  print $ parameterCount network
  print $ loss (sin <$>)
  print $ length training
  mapM_ desc [0..length stops - 1]
  print $ map valB $ concat ysExample
  print $ map valB $ concatMap (predict network (map constB $ snd $ last training)) xsExample