myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (a:as) (b:bs) = (a, b) : myZip as bs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith abToC (a:as) (b:bs) = (abToC a b) : myZipWith abToC as bs

myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 = myZipWith (,)