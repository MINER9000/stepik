module Logger where

import Data.Functor
import Control.Monad

data Log a = Log [String] a
    deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = (\a -> Log [msg] (f a))

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = case f x of
                    Log msg y -> case g y of
                                 Log msg2 z -> Log (msg ++ msg2) z 

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x xs = foldl (>>=) (return x) xs

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msg a) f = case f a of
                        (Log msg2 b) -> Log (msg ++ msg2) b

instance Monad Log where
    return = returnLog
    (>>=)  = bindLog

instance Applicative Log where
    pure  = return
    (<*>) = ap

instance Functor Log where
    fmap = liftM
