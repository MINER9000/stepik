module Main where

import Lib
import Data.Char
import Control.Applicative hiding (many)


main :: IO ()
main = putStrLn "Welcome to MyParser!"

newtype Parser a = Parser { apply :: String -> [(a, String)] }

parse :: Parser a -> String -> a
parse p = fst . head . apply p

anyChar :: Parser Char
anyChar = Parser f where
    f "" = []
    f (c:cs) = [(c,cs)]

instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser fun where
        fun s = [ (f a, s') | (a, s') <- apply p s ]

instance Applicative Parser where
    pure x = Parser $ \s -> [(x, s)]
    pf <*> pv = Parser fun where
        fun s = [(g a, s'') | (g, s') <- apply pf s, (a, s'') <- apply pv s']

satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = Parser f where
    f "" = []
    f (c:cs) | pr c = [(c, cs)]
             | otherwise = []



lower :: Parser Char
lower = satisfy isLower

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

multiplication :: Parser Int
multiplication = (*) <$> digit <* char '*' <*> digit

