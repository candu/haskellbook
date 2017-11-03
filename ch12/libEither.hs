leftToList :: Either a b -> [a]
leftToList (Left a) = [a]
leftToList (Right _) = []

rightToList :: Either a b -> [b]
rightToList (Left _) = []
rightToList (Right b) = [b]

lefts' :: [Either a b] -> [a]
lefts' = foldr ((++) . leftToList) []

rights' :: [Either a b] -> [b]
rights' = foldr ((++) . rightToList) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' [] = ([], [])
partitionEithers' (Left a : xs) =
  let (as, bs) = partitionEithers' xs
  in (a : as, bs)
partitionEithers' (Right b : xs) =
  let (as, bs) = partitionEithers' xs
  in (as, b : bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' bToC (Right b) = Just (bToC b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' aToC _ (Left a) = aToC a
either' _ bToC (Right b) = bToC b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' bToC = either' (const Nothing) (Just . bToC)