module Module3 where

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems a@(x:_) = let (t, s) = span (== x) a in t : groupElems s
