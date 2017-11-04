import Control.Monad
import Data.Char (isAlpha, toLower)
import System.Exit (exitSuccess)

normalize :: String -> String
normalize = map toLower . filter isAlpha

palindrome :: IO ()
palindrome = forever $ do
  line <- getLine
  let line' = normalize line
  case (line' == reverse line') of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess