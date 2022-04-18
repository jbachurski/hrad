module Matrix where

infixl 7 !*!

class Matricial m where
  (!*!) :: m -> m -> m
  matrixTranspose :: m -> m

