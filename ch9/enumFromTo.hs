import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True = [False, True]
eftBool True False = []
eftBool True True = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd start stop
  | start > stop = []
  | start == stop = [stop]
  | otherwise = start : (eftOrd (succ start) stop)

eftInt :: Int -> Int -> [Int]
eftInt start stop
  | start > stop = []
  | start == stop = [stop]
  | otherwise = start : (eftInt (start + 1) stop)

eftChar :: Char -> Char -> [Char]
eftChar start stop
  | start > stop = []
  | start == stop = [stop]
  | otherwise = start : (eftChar (chr (ord start + 1)) stop)

eft :: (Enum a, Eq a, Ord a) => a -> a -> [a]
eft start stop
  | start > stop = []
  | start == stop = [stop]
  | otherwise = start : (eft (succ start) stop)