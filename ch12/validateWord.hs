newtype Word' = Word' String deriving (Eq, Show)

vowels :: [Char]
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel = (flip elem) "aeiou"

countVowels :: String -> Int
countVowels =  length . filter isVowel

mkWord :: String -> Maybe Word'
mkWord s =
  let v = countVowels s
      c = length s - v 
  in
    if v > c then Nothing else Just (Word' s)