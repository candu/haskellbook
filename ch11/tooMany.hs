{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats (Int, String) deriving (Eq, Show)

newtype Goats2 =
  Goats2 (Int, Int) deriving (Eq, Show)

instance TooMany Goats where
  tooMany (Goats (n, _)) = n > 42

instance TooMany Goats2 where
  tooMany (Goats2 (n1, n2)) = (n1 + n2) > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (t1, t2) = tooMany (t1 + t2)