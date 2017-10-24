{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- Num a => a
example = 1

-- Num a => a
a = (* 9) 6

-- Num t => (t, [Char])
b = head [(0,"doge"), (1,"kitteh")]

-- [(Integer, [Char])]
c = [(0 :: Integer ,"doge"),(1,"kitteh")]

-- Bool
d = if False then True else False

-- Int
e = length [1, 2, 3, 4, 5]

-- Bool
f = (length [1, 2, 3, 4]) > (length "TACOCAT")

x = 5
y = x + 5
-- Num a => a
w = y * 10

-- Num a => a -> a
z y = y * 10

-- Fractional a => a
f2 = 4 / y

x2 = "Julie"
y2 = " <3 "
z2 = "Haskell"

-- [Char]
f3 = x2 ++ y2 ++ z2