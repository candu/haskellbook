module Vigenere where

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

shiftForChar :: Char -> Int
shiftForChar c
  | isLower c = ord c - minLower
  | isUpper c = ord c - minUpper
  | otherwise = 0

encodePair :: (Char, Char) -> Char
encodePair (c1, c2) = shiftChar (shiftForChar c1) c2

decodePair :: (Char, Char) -> Char
decodePair (c1, c2) = shiftChar (- (shiftForChar c1)) c2

encode :: String -> String -> String
encode key msg =
  map encodePair (zip (foldr (++) "" (repeat key)) msg)

decode :: String -> String -> String
decode key msg =
  map decodePair (zip (foldr (++) "" (repeat key)) msg)