module Main where

import Data.List (sort)
import Test.QuickCheck
import Test.QuickCheck.Function (Fun, apply)

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

prop_halfIdentity :: (Fractional a, Eq a) => a -> Bool
prop_halfIdentity x = halfIdentity x == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status @(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered = listOrdered . sort

plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y =
  x + y == y + x

prop_plusAssociative :: (Int, Int, Int) -> Bool
prop_plusAssociative (x, y, z) = plusAssociative x y z

prop_plusCommutative :: (Int, Int) -> Bool
prop_plusCommutative (x, y) = plusCommutative x y

timesAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
timesAssociative x y z =
  x * (y * z) == (x * y) * z

timesCommutative :: (Num a, Eq a) => a -> a -> Bool
timesCommutative x y =
  x * y == y * x

prop_timesAssociative :: (Int, Int, Int) -> Bool
prop_timesAssociative (x, y, z) = timesAssociative x y z

prop_timesCommutative :: (Int, Int) -> Bool
prop_timesCommutative (x, y) = timesCommutative x y

prop_quotRem :: (Int, NonZero Int) -> Bool
prop_quotRem (x, NonZero y) = (quot x y) * y + (rem x y) == x

prop_divMod :: (Int, NonZero Int) -> Bool
prop_divMod (x, NonZero y) = (div x y) * y + (mod x y) == x

powAssociative :: Int -> Int -> Int -> Bool
powAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

powCommutative :: Int -> Int -> Bool
powCommutative x y =
  x ^ y == y ^ x

prop_powAssociative :: (Int, Int, Int) -> Bool
prop_powAssociative (x, y, z) = powAssociative x y z

prop_powCommutative :: (Int, Int) -> Bool
prop_powCommutative (x, y) = powCommutative x y

prop_reverse :: Eq a => [a] -> Bool
prop_reverse xs = (reverse (reverse xs)) == xs

prop_dollarsign :: Eq b => (Fun a b, a) -> Bool
prop_dollarsign (f, a) = (apply f $ a) == (apply f a)

prop_compose :: Eq c => (Fun b c, Fun a b, a) -> Bool
prop_compose (f, g, a) = ((apply f) . (apply g)) a == (apply f (apply g a))

prop_foldrColon :: Eq a => ([a], [a]) -> Bool
prop_foldrColon (z, xs) = foldr (:) z xs == (++) z xs

prop_foldrConcat :: Eq a => [[a]] -> Bool
prop_foldrConcat xss = foldr (++) [] xss == concat xss

prop_length :: (Int, [a]) -> Bool
prop_length (n, xs) = length (take n xs) == n

prop_readShow :: (Eq a, Read a, Show a) => a -> Bool
prop_readShow x = (read (show x)) == x

main :: IO ()
main = do
  quickCheck (prop_halfIdentity :: Double -> Bool)
  quickCheck (prop_listOrdered :: [Int] -> Bool)
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_timesAssociative
  quickCheck prop_timesCommutative
  quickCheck prop_quotRem
  quickCheck prop_divMod
  quickCheck prop_powAssociative
  quickCheck prop_powCommutative
  quickCheck (prop_reverse :: [Int] -> Bool)
  quickCheck (prop_dollarsign :: (Fun Int Int, Int) -> Bool)
  quickCheck (prop_compose :: (Fun Int Int, Fun Int Int, Int) -> Bool)
  quickCheck (prop_foldrColon :: ([Int], [Int]) -> Bool)
  quickCheck (prop_foldrConcat :: [[Int]] -> Bool)
  quickCheck (prop_length :: (Int, [Int]) -> Bool)
  quickCheck (prop_readShow :: [Int] -> Bool)