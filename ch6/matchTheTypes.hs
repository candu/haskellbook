import Data.List (sort)

i :: Num a => a
-- i :: a fails
i = 1

f :: RealFrac a => a
-- f :: Num a => a fails
f = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int
sigmund :: Int -> Int
-- sigmund :: a -> a fails
sigmund x = myX

sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a fails
sigmund' x = myX

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort
signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a fails
signifier xs = head (mySort xs)