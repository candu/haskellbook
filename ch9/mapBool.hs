import Data.Bool

mapBool :: (a -> b) -> (a -> b) -> (a -> Bool) -> [a] -> [b]
mapBool fFalse fTrue fCond xs =
  map (\x -> bool (fFalse x) (fTrue x) (fCond x)) xs