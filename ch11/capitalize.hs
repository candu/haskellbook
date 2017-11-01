import Data.Char
import Data.List

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs) = toUpper c : cs

capitalizeWordTuple :: String -> (String, String)
capitalizeWordTuple "" = ("", "")
capitalizeWordTuple s@(c:cs) = (s, toUpper c : cs)

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map capitalizeWordTuple . words

capitalizeHelper :: Bool -> [String] -> [String]
capitalizeHelper _ [] = []
capitalizeHelper firstWord (s : ss) = s' : (capitalizeHelper firstWord' ss)
  where s' = if firstWord then (capitalizeWord s) else s
        firstWord' = isSuffixOf "." s

capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . (capitalizeHelper True) . words