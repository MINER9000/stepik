module Module2 where

import Data.Function

--multSecond = g `on` h

--g = (+)

--h = snd

doItYourself = f . g . h

f = logBase 2

g = (^3)

h = max 42

class Printable a where
    toString :: a -> String
    
instance Printable Bool where
    toString True = "True"
    toString False = "False"
    
instance Printable () where
    toString _ = "unit type"

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x | doesEnrageGork x && doesEnrageMork x = stomp $ stab x
                  | doesEnrageGork x = stomp x
                  | doesEnrageMork x = stab x
                  | otherwise = x

ip = show a ++ show b ++ show c ++ show d

a = 127.2
b = 24.1
c = 20.1
d = 2
