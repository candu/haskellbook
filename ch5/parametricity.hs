-- parametricity.hs
module Parametricity where

f1 :: a -> a -> a
f1 a1 a2 = a1

f2 :: a -> a -> a
f2 a1 a2 = a2

g :: a -> b -> b
g a b = b