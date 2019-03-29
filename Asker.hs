module Asker where

import System.IO (hFlush, stdout)
import Data.Char (isSpace)

main :: IO ()
main = do
    say "Hello Stranger!"
    say "Could you tell me you name?"
    name <- requestName
    say $ "Wow! Such a glourious name " ++ name ++ "!"

say :: String -> IO ()
say = putStrLn . ("> " ++)

req :: IO String
req = do
    putStr $ "> "
    hFlush stdout
    s <- getLine
    return s

requestName :: IO String
requestName = do
    name <- req
    let name' = trim name
    if correctName name' then
        return name'
    else do 
        say "Could you, please, tell me your real name"
        requestName

correctName :: String -> Bool
correctName s = case s of
              ""       -> False
              "qwerty" -> False
              _        -> True

trim :: String -> String
trim = reverse . trimSpaces . reverse . trimSpaces

trimSpaces :: String -> String
trimSpaces [] = []
trimSpaces (x:xs) | isSpace x = trimSpaces xs
                  | otherwise = x:xs
