data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case (f a) of
  Nothing -> Leaf
  Just (a1, b, a2) -> Node (unfold f a1) b (unfold f a2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold go 0
  where
    go k =
      if k == n then Nothing
      else Just (k + 1, k, k + 1)

main :: IO ()
main = do
  print $ treeBuild 2