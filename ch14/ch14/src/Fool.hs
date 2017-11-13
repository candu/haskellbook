module Fool where

import Test.QuickCheck.Gen (Gen, oneof, frequency)

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

foolGenEqual :: Gen Fool
foolGenEqual = do
  oneof [return Fulse, return Frue]

foolGenBiased :: Gen Fool
foolGenBiased = do
  frequency [(2, return Fulse),
             (1, return Frue)]