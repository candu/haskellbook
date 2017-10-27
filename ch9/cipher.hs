module Cipher where

import Data.Char

minLower :: Int
minLower = ord 'a'

minUpper :: Int
minUpper = ord 'A'

shiftLower :: Int -> Char -> Char
shiftLower k c =
  let n = ord c - minLower
      n' = (n + k) `mod` 26
  in chr (minLower + n')

shiftUpper :: Int -> Char -> Char
shiftUpper k c =
  let n = ord c - minUpper
      n' = (n + k) `mod` 26
  in chr (minUpper + n')

shiftChar :: Int -> Char -> Char
shiftChar k c
  | isLower c = shiftLower k c
  | isUpper c = shiftUpper k c
  | otherwise = c

caesar :: Int -> String -> String
caesar k = map (shiftChar k)

unCaesar :: Int -> String -> String
unCaesar k = map (shiftChar (-k))