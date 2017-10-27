myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . (map f)

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (== x)

myReverse :: [a] -> [a]
myReverse = foldl (\xs x -> x : xs) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . (map f)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "cannot max empty list!"
myMaximumBy f (x:xs) =
  let f' = \a b -> if f a b == GT then a else b
  in foldr f' x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "cannot min empty list!"
myMinimumBy f (x:xs) =
  let f' = \a b -> if f a b == LT then a else b
  in foldr f' x xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare