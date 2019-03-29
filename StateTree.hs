module StateTree where

import Control.Monad.Trans.State

data Tree a = Leaf a
            | Fork (Tree a) a (Tree a)
    deriving Show

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (numberTree' tree) 0

numberTree' :: Tree () -> State Integer (Tree Integer)
numberTree' tree = do
    case tree of
        (Leaf _) -> do
            modify (+1)
            n <- get
            return $ Leaf n
        (Fork l _ r) -> do
            modL <- numberTree' l
            modify (+1)
            n <- get
            modR <- numberTree' r
            return $ Fork modL n modR
