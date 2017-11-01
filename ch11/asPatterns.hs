isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] [] = True
isSubseqOf _ [] = False
isSubseqOf [] _ = True
isSubseqOf xx@(x:xs) (y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf xx ys

main :: IO ()
main = do
  print $ isSubseqOf "blah" "blahwoot"
  print $ isSubseqOf "blah" "wootblah"
  print $ isSubseqOf "blah" "wboloath"
  print $ not (isSubseqOf "blah" "wootbla")
  print $ not (isSubseqOf "blah" "halbwoot")
  print $ isSubseqOf "blah" "blawhoot"
  print $ isSubseqOf "blah" "blablah"