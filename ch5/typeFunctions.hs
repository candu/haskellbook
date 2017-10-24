module TypeFunctions where

i :: a -> a
i a = a

c :: a -> b -> a
c a b = a

c'' :: b -> a -> b
c'' b a = b

c' :: a -> b -> b
c' a b = b

r1 :: [a] -> [a]
r1 (x : xs) = xs

r2 :: [a] -> [a]
r2 = reverse

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

a :: (a -> c) -> a -> a
a aToC a = a

a' :: (a -> b) -> a -> b
a' aToB a = aToB a