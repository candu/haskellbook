tensDigit :: Integral a => a -> a
tensDigit x = snd (divMod (div x 10) 10)

hunsD :: Integral a => a -> a
hunsD x = snd (divMod (div x 100) 10)

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase a1 a2 b =
  case b of
    True -> a1
    False -> a2

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard a1 a2 b
  | b = a1
  | otherwise = a2

g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c)