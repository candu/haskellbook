import Data.Char

ucfirst :: String -> String
ucfirst "" = ""
ucfirst (c:cs) = toUpper c : cs

toUpperCase :: String -> String
toUpperCase = map toUpper

uchead :: String -> Char
uchead = head . (map toUpper)