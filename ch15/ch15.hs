import Control.Monad
import Data.Semigroup
import Test.QuickCheck

-- MyPositive

newtype MyPositive = MyPositive' Int
  deriving (Eq, Show)

getMyPositive :: Int -> MyPositive
getMyPositive x
  | x < 0 = MyPositive' (-x)
  | x == 0 = MyPositive' 1
  | otherwise = MyPositive' x

instance Arbitrary MyPositive where
  arbitrary = do
    x <- arbitrary
    return (getMyPositive x)

instance Semigroup MyPositive where
  (MyPositive' a) <> (MyPositive' b) = MyPositive' (max a b)

-- Or

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary =
    frequency [(1, liftM Fst arbitrary),
               (1, liftM Snd arbitrary)]

instance Semigroup (Or a b) where
  (Fst _) <> y = y
  x <> (Fst _) = x
  x@(Snd _) <> (Snd _) = x

-- Combine

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Eq (Combine a b) where
  (Combine _) == (Combine _) = True

instance Show (Combine a b) where
  show (Combine { unCombine = _ }) = "Combine <function>"

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return Combine { unCombine = f }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine { unCombine = f }) <> (Combine { unCombine = g }) =
    Combine { unCombine = \a' -> f a' <> g a' }

-- TESTS

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: MyPositive -> MyPositive -> MyPositive -> Bool)
  quickCheck (semigroupAssoc :: Or Int String -> Or Int String -> Or Int String -> Bool)
  quickCheck (semigroupAssoc :: Combine Int String -> Combine Int String -> Combine Int String -> Bool)