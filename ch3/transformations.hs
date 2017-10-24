-- transformations.hs
module Transformations where

exclaim :: String -> String
exclaim s = s ++ "!"

-- I know, I know, we haven't covered pattern matching or Maybe yet.
-- This still seemed like the most useful generalization of the rule,
-- and I needed *some* kind of case structure to handle it.  The Maybe
-- stuff is just pure curiosity on my part.
lastLetterOfFirstWord :: String -> Maybe Char
lastLetterOfFirstWord "" = Nothing
lastLetterOfFirstWord (c : ' ' : cs) = Just c
lastLetterOfFirstWord (c : "") = Just c
lastLetterOfFirstWord (c : cs) = lastLetterOfFirstWord cs

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn x [] = [[]]
splitOn x xs = let (prefix, suffix) = span (x /=) xs
               in case suffix of
                 [] -> [prefix]
                 _  -> prefix : (splitOn x (tail suffix))


lastWord :: String -> String
lastWord s = last (splitOn ' ' s)