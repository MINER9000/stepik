module Module3 where

import Data.List (unfoldr)

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems a@(x:_) = let (t, s) = span (== x) a in t : groupElems s

perms :: [a] -> [[a]]
perms [] = [[]]
perms p = concatMap (\ (x:xs) -> map (x :) (perms xs)) (helper p p)
  where
    helper :: [a] -> [a] -> [[a]]
    helper _ [] = []
    helper (x:xs) (y:ys) = let p = (xs++[x]) in p : helper p ys

fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

data Odd = Odd Integer deriving (Eq,Show)
-- не убирайте комментарий с предыдущей строки
-- определение Odd уже присутствует в вызывающей программе
oddVal :: Odd -> Integer
oddVal (Odd n) = n
instance Enum Odd where
    succ (Odd n) = Odd (n + 2)
    pred (Odd n) = Odd (n - 2)
    fromEnum (Odd n) = fromInteger n
    toEnum n = (Odd (toInteger n)) 
    enumFrom n = n : enumFrom (succ n)
    enumFromThen n@(Odd v1) c@(Odd v2) 
                 = n : enumFromThen c (Odd (2*v2 - v1))
    enumFromTo a1 a2 | oddVal a1 > oddVal a2 = []
                     | otherwise             = a1 : enumFromTo (succ a1) a2
    enumFromThenTo a1@(Odd v1) a2@(Odd v2) a3@(Odd v3) 
        | v2 - v1 > 0 && v3 < v1 = []
        | v2 - v1 < 0 && v3 > v1 = []
        | otherwise = a1 : enumFromThenTo a2 (Odd (2 * v2 - v1)) a3

coins :: (Ord a, Num a) => [a]
coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
change n | n < 0     = []
         | n == 0    = [[]]
         | otherwise = [ x : xs | x <- coins, xs <- change (n - x) ]

evenOnly :: [a] -> [a]
evenOnly [] = []
evenOnly [_] = []
evenOnly (x:y:xs) = y : evenOnly xs

evenOnly' :: [a] -> [a]
evenOnly' = snd . foldr (\x (ys, xs) -> (xs, x : ys)) ([], [])

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g
  where g (a, b) | b < a     = Nothing
                  | otherwise = Just (b, (a, pred b))
