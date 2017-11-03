module Main where

import qualified Data.ByteString.Lazy as LBS
import System.IO

import Codec.Compression.GZip (decompress)
import Control.Monad (forever)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- fmap (unpack . decompress) (LBS.readFile "data/dict.txt.gz")
  return (lines dict)

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in     l >= minWordLength
              && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
      fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) ""

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) = (flip elem) word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) = (flip elem) guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word discovered guessed) c =
  Puzzle word discovered' (c : guessed)
  where
    zipper wordChar guessChar
      | wordChar == c = Just wordChar
      | otherwise = guessChar
    discovered' = zipWith zipper word discovered

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle c = do
  putStrLn $ "Your guess was: " ++ [c]
  case (charInWord puzzle c, alreadyGuessed puzzle c) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "Yes, it's in the word!"
      return (fillInCharacter puzzle c)
    (False, _) -> do
      putStrLn "Nope, not in the word."
      return (fillInCharacter puzzle c)

maxGuesses :: Int
maxGuesses = 7

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ guessed) =
  if (length guessed) > 7 then
    do putStrLn "You lose :("
       putStrLn $ "The word was: " ++ word
       exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ discovered _) =
  if all isJust discovered then
    do putStrLn "You win!"
       exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character."

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
