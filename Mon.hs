module Mon where

import Data.Monoid

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
    mappend (Maybe' Nothing) (Maybe' (Just b)) = Maybe' (Nothing)
    mappend (Maybe' (Just a)) (Maybe' Nothing) = Maybe' (Nothing)
    mappend (Maybe' (Just a)) (Maybe' (Just b)) = 
            Maybe' (Just (a `mappend` b))
