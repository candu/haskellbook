import Control.Monad
import Data.Monoid
import Test.QuickCheck

import Optional

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary =
    frequency [(1, return First' { getFirst' = Nada }),
               (3, do
                 x <- liftM Only arbitrary
                 return First' { getFirst' = x })]

instance Monoid a => Monoid (First' a) where
  mempty = First' { getFirst' = Nada }
  mappend (First' { getFirst' = Only x })
          (First' { getFirst' = Only y }) =
            First' { getFirst' = Only (mappend x y) }
  mappend (First' { getFirst' = Nada }) y = y
  mappend x (First' { getFirst' = Nada }) = x

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (ma :: First' (Sum Int) -> First' (Sum Int) -> First' (Sum Int) -> Bool)
  quickCheck (mli :: First' (Sum Int) -> Bool)
  quickCheck (mlr :: First' (Sum Int) -> Bool)