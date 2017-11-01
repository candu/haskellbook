fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN x = fibs !! x

fibs20 :: [Integer]
fibs20 = take 20 fibs

fibsLessThan100 :: [Integer]
fibsLessThan100 = takeWhile (\x -> x < 100) fibs

factorial :: [Integer]
factorial = scanl (*) 1 (iterate (+1) 1)