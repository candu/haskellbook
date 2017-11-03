import qualified Data.Char as Char

-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe = unwords . go . (map notThe) . words
  where
    go [] = []
    go (Nothing : ws) = "a" : go ws
    go (Just w : ws) = w : go ws

isVowel :: Char -> Bool
isVowel c = elem (Char.toLower c) "aeiou"

-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where
    go [] = 0
    go (_ : []) = 0
    go ("the" : ws)
      | isVowel (head (head ws)) = 1 + go ws
      | otherwise = go ws
    go (_ : ws) = go ws

-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4
countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel