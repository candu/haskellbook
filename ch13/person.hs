type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
  -> Age
  -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Name: "
  name <- getLine
  putStr "Age: "
  ageStr <- getLine
  -- We could read this as a Maybe Integer with Text.Read.readMaybe
  let age = read ageStr :: Integer
  case (mkPerson name age) of
    Left err -> putStrLn $ show err
    Right person -> putStrLn $ "Yay! Successfully got a person: " ++ (show person)