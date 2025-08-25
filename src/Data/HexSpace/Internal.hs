module Data.HexSpace.Internal (Cube (..), Torsor (..)) where

-- | A hex position in cube coordinates where q + r + s = 0
data Cube a = Cube a a a deriving (Show, Eq, Functor)

-- | A vector between hex positions
data Torsor a = Torsor a a a deriving (Show, Eq, Functor)

