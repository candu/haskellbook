-- arith4.hs
module Arith4 where

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read b) => a -> b
roundTrip a = read (show a)

roundTripPF :: (Show a, Read b) => a -> b
roundTripPF = read . show

main :: IO ()
main = do
  print (roundTrip 4 :: Integer)
  print (roundTripPF 4 :: Integer)
  print (id 4)