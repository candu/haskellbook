module PoemLines where

firstSen :: String
firstSen = "Tyger Tyger, burning bright\n"

secondSen :: String
secondSen = "In the forests of the night\n"

thirdSen :: String
thirdSen = "What immortal hand or eye\n"

fourthSen :: String
fourthSen = "Could frame thy fearful\
            \ symmetry?"

sentences :: String
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

mySplit :: Char -> String -> [String]
mySplit _ [] = []
mySplit c s@(c' : cs)
  | c == c' = mySplit c cs
  | otherwise = (takeWhile (/= c) s) : mySplit c (dropWhile (/= c) s)

myLines :: String -> [String]
myLines = mySplit '\n'

shouldEqual :: [String]
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]


main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences == shouldEqual)