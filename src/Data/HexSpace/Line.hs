module Data.HexSpace.Line (Line(..), hexes, intersect, Beam(..), intersectBeam) where



import Data.HexSpace.Coordinates
import Data.List (genericTake)
import Data.Ratio ((%))

import Test.QuickCheck.Arbitrary

import Prelude hiding (any,all,or,and)
import Control.Applicative
import Control.Monad (guard)

data Line a = Line { start :: Cube a, end :: Cube a }
  deriving (Show, Eq)

instance (Num a, Arbitrary a) => Arbitrary (Line a) where
    arbitrary = Line <$> arbitrary <*> arbitrary

data Beam a = Beam { origin :: Cube a, direction :: Torsor a }
  deriving (Show, Eq)

instance (Num a, Arbitrary a) => Arbitrary (Beam a) where
    arbitrary = Beam <$> arbitrary <*> arbitrary

at :: (Num a) => Beam a -> a -> Cube a
at (Beam o d) t = o <+> scale t d

intersectBeam :: (Fractional a, Eq a, Alternative f) => Beam a -> Beam a -> f (Cube a, a, a)
intersectBeam b1 b2 = do 
        let det = cross (direction b1) (direction b2)
        let d = origin b2 <-> origin b1
        let detMagSquared = det <.> det
        guard $ det /= torsor 0 0 0
        let t = (cross d (direction b2) <.> det) / detMagSquared
        let u = (cross d (direction b1) <.> det) / detMagSquared
        pure (b1 `at` t, t, u)

-- | The hexes in a line. Note: The entire line is not necessarily contained in the hexes.
hexes :: (Integral a) => Line a -> [Cube a]
hexes l = let v = end l <-> start l
              n = distance v
              dv = (% n) <$> v
           in genericTake (n + 1) $ nearestHexR <$> iterate (<+>dv) (fromIntegral <$> start l)


intersect :: (Integral a) => Line a -> Line a -> Maybe (Cube a)
intersect _ _ = undefined -- TODO


