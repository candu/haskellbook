import qualified Data.Char as Char
import Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

type PhoneKey = (Digit, [Char])

data PhoneKeyboard = PhoneKeyboard {
  keys :: [PhoneKey]
} deriving (Eq, Show)

phoneKeyboard :: PhoneKeyboard
phoneKeyboard = PhoneKeyboard [
  ('1', ""),
  ('2', "abc"),
  ('3', "def"),
  ('4', "ghi"),
  ('5', "jkl"),
  ('6', "mno"),
  ('7', "pqrs"),
  ('8', "tuv"),
  ('9', "wxyz"),
  ('0', " +_"),
  ('#', ".,")]

data PhonePresses = PhonePresses {
  chars :: Map Char (Digit, Presses)
} deriving (Eq, Show)

makePhonePresses :: PhoneKey -> [(Char, (Digit, Presses))]
makePhonePresses (d, cs) =
  [(d', (d, p)) | (d', p) <- zip (cs ++ [d]) [1..]]

phonePresses :: PhonePresses
phonePresses = 
  PhonePresses (
    Map.fromList . concat . (map makePhonePresses) . keys
    $ phoneKeyboard)

reverseTaps :: PhonePresses -> Char -> [(Digit, Presses)]
reverseTaps phone c
  | Char.isUpper c = ('*', 1) : (reverseTaps phone (Char.toLower c))
  | otherwise = [(chars phone) ! c]

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: PhonePresses
  -> String
  -> [(Digit, Presses)]
cellPhonesDead phone = concat . (map $ reverseTaps phone)

convo :: [String]
convo = [
  "Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Just making sure rofl ur turn"]

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

groupTaps :: [(Char, Presses)] -> (Presses, Char)
groupTaps taps = (sum $ map snd taps, fst $ head taps)

groupTapsBy :: (Char, Presses) -> (Char, Presses) -> Bool
groupTapsBy (c, _) (c', _) = c == c'

mostExpensiveLetter :: String -> Char
mostExpensiveLetter s =
  let taps = zip s (map (fingerTaps . reverseTaps phonePresses) s)
      groupedTaps = map groupTaps (groupBy groupTapsBy (List.sort taps))
  in
  snd . head . (List.sortBy (flip compare)) $ groupedTaps

main :: IO ()
main = do
  mapM_ (print . mostExpensiveLetter) convo