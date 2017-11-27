import Data.Monoid

newtype Mem s a =
  Mem { runMem :: s -> (a, s) }

memCombine :: Monoid a => (s -> (a, s)) -> (s -> (a, s)) -> s -> (a, s)
memCombine f g s' =
  let (a', s'') = f s'
      (a'', s''') = g s''
  in (a' <> a'', s''')

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s0 -> (mempty, s0)
  mappend (Mem f) (Mem g) = Mem (memCombine f g)

f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0