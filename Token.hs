module Token where

import Data.Char (isDigit)
import Data.Maybe (isNothing, catMaybes)
import Data.Monoid

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken []  = Nothing
asToken [x] | isDigit x = Just $ Number $ read [x]
            | x == '+'  = Just Plus
            | x == '-'  = Just Minus
            | x == '('  = Just LeftBrace
            | x == ')'  = Just RightBrace
            | otherwise = Nothing
asToken lst | all isDigit lst = Just $ Number $ read lst
            | otherwise       = Nothing

tokenize :: String -> Maybe [Token]
tokenize input = do
    let tokens = map asToken $ words input
    foldr1 (>>) tokens
    return $ catMaybes tokens
