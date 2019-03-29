module ParsePerson where

import Data.List (findIndex)
import Data.Char (isDigit)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String
    deriving Show
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int }
    deriving Show

parsePerson :: String -> Either Error Person
parsePerson input = case constructPerson input of
                    (Right (p, 3)) -> Right p
                    (Left err)     -> Left err
                    (Right (p, _)) -> Left IncompleteDataError

constructPerson :: String -> Either Error (Person, Int)
constructPerson = (parseValue newPerson) . (map $ splitOn '=') . lines
  where newPerson = (Person{}, 0)

parseValue :: (Person, Int)
           -> [Either Error (String, String)]
           -> Either Error (Person, Int)
parseValue p []          = Right p
parseValue _ ((Left err):_) = Left err
parseValue (p, cnt) ((Right (var, val)):xs)
    | var == "firstName" = parseValue (p{firstName = val}, cnt + 1) xs
    | var == "lastName"  = parseValue (p{lastName  = val}, cnt + 1) xs
    | var == "age"
      && all isDigit val = parseValue (p{age = read val}, cnt + 1) xs
    | var == "age"       = Left (IncorrectDataError val)
    | otherwise          = parseValue (p, cnt) xs

splitOn :: Char -> String -> Either Error (String, String)
splitOn c s = case elemIndex c s of
              Nothing -> Left ParsingError
              Just x  -> let (a, b) = splitAt x s
                         in Right (trim a, trim $ tail b)

trim :: String -> String
trim = reverse . trim' . reverse . trim'
  where
    trim' []        = []
    trim' (x:xs) = if x == ' '
                   then trim' xs
                   else x:xs
