module Example where

import System.Random
import Data.List
import Control.Monad

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import GeneralDiff
import NeuralNet

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

relu :: Fractional a => a -> a
relu x = (abs x + x) / 2

reluB :: (Ord a, Num a) => B a -> B a
reluB x@(B v _) = if v >= 0 then x else 0

leakyReluB :: (Ord a, Fractional a) => B a -> B a
leakyReluB x@(B v _) = if v >= 0 then x else x/100

networkArch :: [Layer (B Double)]
networkArch = [Perceptron 1 3 [], Activation sigmoid, Perceptron 3 3 [], Activation sigmoid, Perceptron 3 1 []]

initWeights n = replicateM n (randomRIO (-2,2::Double))

fun = sin

xsExample = map singleton [-3,-2.75..3] :: [[Double]]
ysExample = map fun <$> xsExample
loss = mse (map constB <$> xsExample) (map constB <$> ysExample)

xsSignal = [-3.25,-3.125..3.25] :: [Double]
ysSignal = map fun xsSignal

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

run = do
  setStdGen $ mkStdGen 1337
  paramsIni <- initWeights (parameterCount networkArch)
  print "Hello!"
  let
    networkIni = networkWithParameters networkArch (map constB paramsIni)
    stops = [0, 1, 2, 5, 10, 20, 50, 100, 200, 1000, 5000] ++ [10000,20000..100000]
    training = zip stops $ reps (networkGradientStep loss 2e-3) networkIni stops
    final = snd $ last training
    predictFinal = valB . (!! 0) . predict final . map constB . singleton
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
  toFile def "example.png" $ do
    layout_title .= "Trained function"
    setColors [opaque blue, opaque cyan, opaque red]
    plot (line "baseline" [zip xsSignal ysSignal])
    plot (line "trained" [zip xsSignal (map predictFinal xsSignal)])
    plot (points "examples" (zip (map (!! 0) xsExample) (map (!! 0) ysExample)))
