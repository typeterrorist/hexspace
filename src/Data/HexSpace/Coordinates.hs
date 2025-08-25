{-|
Module      : Data.Hexes.Coordinates
Description : Cube coordinates for heaxagonal grids.
Copyright   : (c) 2025
License     : BSD3

A library for working with hexagonal grids using cube coordinates.
Provides operations for hex manipulation, pathfinding, and grid operations.
-}
module Data.HexSpace.Coordinates
  ( Cube
  , Torsor
  , (<+>)
  , (<->)
  , (<.>)
  , axial
  , bearing
  , cardinal
  , cardinals
  , cardinalsClockwise
  , cartesian
  , cartesianTorsor
  , component
  , CornerDirection(..)
  , cross
  , cube
  , cubeX
  , cubeY
  , cubeCoordinates
  , torsorCoordinates
  , ccwEnd
  , cwEnd
  , after
  , before
  , Direction(..)
  , distance
  , distanceF
  , facing
  , fromCartesian
  , fromSpiral
  , hexCorner
  , hexCornerI
  , isAdjacent
  , nearestHex
  , nearestHexR
  , orientation
  , origo
  , primitiveDirection
  , ring
  , rotateCCW
  , rotateCW
  , roundDiff
  , scale
  , torsor
  , torsorX
  , torsorY
  , toSpiral
  , unitHexCorner
  , xtorsor
  , ytorsor
  ) where


import Data.List (maximumBy)
import Data.Ratio (numerator, denominator, Ratio, (%))
import Data.Function (on)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (elements)

import qualified Data.Map as Map
import Data.Map (Map)

import Data.HexSpace.Internal




-- | Create a canonical cube coordinate where q + r + s = 0
cube :: (Num a) => a -> a -> a -> Cube a
cube q r s = Cube (s - r) (q - s) (r - q)


-- | Create axial coordinate Cube

axial :: (Num a) => a -> a -> Cube a
axial x y = Cube x y (-x - y)

-- | Create a canonical direction vector
torsor :: (Num a) => a -> a -> a -> Torsor a
torsor dq dr ds = Torsor (ds -dr) (dq - ds) (dr - dq)

{-

     NW  ╱ ╲ NE
       ╱     ╲
      │       │
    W │   o   │ E
      │       │
       ╲     ╱
    SW   ╲ ╱ SE


   Legend:
   O: Center hex
   NW: Northwest
   NE: Northeast
   E: East
   SE: Southeast
   SW: Southwest
   W: West
-}

data Direction = NW | NE | E | SE | SW | W
  deriving (Show, Eq, Ord, Enum, Bounded)

data CornerDirection = NC | NEC | SEC | SC | SWC | NWC
  deriving (Show, Eq, Ord, Enum, Bounded)

cwEnd :: Direction -> CornerDirection
cwEnd NW = NC
cwEnd NE = NEC
cwEnd E = SEC
cwEnd SE = SC
cwEnd SW = SWC
cwEnd W = NWC

ccwEnd :: Direction -> CornerDirection
ccwEnd NW = NWC
ccwEnd NE = NC
ccwEnd E = NEC
ccwEnd SE = SEC
ccwEnd SW = SC
ccwEnd W = SWC

before :: CornerDirection -> Direction
before NC = NW
before NEC = NE
before SEC = E
before SC = SE
before SWC = SW
before NWC = W

after :: CornerDirection -> Direction
after NC = NE
after NEC = E
after SEC = SE
after SC = SW
after SWC = W
after NWC = NW

instance Arbitrary Direction where
  arbitrary = elements [NW .. W]

cardinal :: (Num a) => Direction -> Torsor a
cardinal direction = case Map.lookup direction cardinals of
  Just t -> t
  Nothing -> error "Invalid direction (THIS IS A BUG)"

cardinals :: (Num a) => Map Direction (Torsor a)
cardinals = Map.fromList $ zip [NW .. W] cardinalsClockwise


-- | Unit vectors for the six cardinal directions
cardinalsClockwise :: (Num a) => [Torsor a]
cardinalsClockwise
           = [ torsor 1 0 0     -- ^ Northwest
             , torsor 0 0 (-1)  -- ^ Northeast
             , torsor 0 1 0     -- ^ East
             , torsor (-1) 0 0  -- ^ Southeast
             , torsor 0 0 1     -- ^ Southwest
             , torsor 0 (-1) 0  -- ^ East
             ]

component :: (Num a, Integral a) => Torsor a -> Direction -> a
component (Torsor dq dr ds) direction = case direction of
    NW -> (dr - ds) -- q
    NE -> (dr - dq)
    E  -> (ds - dq)  -- r
    SE -> (ds - dr)
    SW -> (dq - dr)  -- s
    W  -> (dq - ds)

cubeCoordinates :: Cube a -> (a,a,a)
cubeCoordinates (Cube q r s) = (q,r,s)

torsorCoordinates :: Torsor a -> (a,a,a)
torsorCoordinates (Torsor q r s) = (q, r, s)

-- | Compute the X component of a direction vector
torsorX :: Fractional a => Torsor a -> a
torsorX (Torsor dq _ ds) = (ds - dq)/2

torsorY :: Floating a => Torsor a -> a
torsorY (Torsor dq dr ds) = sqrt 3 * (2*dr - ds - dq) / 6

cubeX :: Fractional a => Cube a -> a
cubeX (Cube q _ s) = (s - q)/2

cubeY :: Floating a => Cube a -> a
cubeY (Cube q r s) = sqrt 3 * (2*r - s - q) / 6

cartesian :: Floating a => Cube a -> (a, a)
cartesian c = (cubeX c, cubeY c)

cartesianTorsor :: Floating a => Torsor a -> (a, a)
cartesianTorsor d = (torsorX d, torsorY d)

xtorsor :: Fractional a => Torsor a
xtorsor = cardinal E
ytorsor :: Floating a => Torsor a
ytorsor = scale (1/sqrt 3) (cardinal NE + cardinal NW)

fromCartesian :: Floating a => (a, a) -> Cube a
fromCartesian (x, y) = (origo <+> scale x xtorsor) <+> scale y ytorsor

unitHexCorner :: (Fractional a) => CornerDirection -> Torsor a
unitHexCorner direction = scale (1/3) (cardinal (before direction) + cardinal (after direction))

hexCorner :: (Fractional a) => CornerDirection -> Cube a -> Cube a
hexCorner = (flip (<+>)) . unitHexCorner

hexCornerI :: (Fractional a, Integral b) => CornerDirection -> Cube b -> Cube a
hexCornerI =  (. (fromIntegral <$>)) . hexCorner

instance (Num a) => Num (Torsor a) where
    (Torsor dq dr ds) + (Torsor dq' dr' ds') = Torsor (dq + dq') (dr + dr') (ds + ds')
    (Torsor dq dr ds) - (Torsor dq' dr' ds') = Torsor (dq - dq') (dr - dr') (ds - ds')
    fromInteger n = scale (fromInteger n) (torsor 1 0 0)
    (*) = error "multiplication not implemented for Torsor"
    abs = error "abs not implemented for Torsor"
    signum = error "signum not implemented for Torsor"

instance (Arbitrary a, Num a) => Arbitrary (Cube a) where
  arbitrary = cube <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Num a) => Arbitrary (Torsor a) where
  arbitrary = torsor <$> arbitrary <*> arbitrary <*> arbitrary

-- | Add a vector to a position
(<+>) :: (Num a) => Cube a -> Torsor a -> Cube a
(Cube q r s) <+> (Torsor dq dr ds) = Cube (q + dq) (r + dr) (s + ds)

-- | Vector between two positions
(<->) :: (Num a) => Cube a -> Cube a -> Torsor a
(Cube q r s) <-> (Cube q' r' s') = Torsor (q - q') (r - r') (s - s')

-- | Dot product of vectors
(<.>) :: (Num a) => Torsor a -> Torsor a -> a
(Torsor dq dr ds) <.> (Torsor dq' dr' ds') = dq * dq' + dr * dr' + ds * ds'

-- | Cross product of vectors
cross :: (Num a) => Torsor a -> Torsor a -> Torsor a
(Torsor dq dr ds) `cross` (Torsor dq' dr' ds') = Torsor (dr * ds' - ds * dr') (ds * dq' - dq * ds') (dq * dr' - dr * dq')


-- | Fast ring (distance to the origin).  Max-norm is identical to the
--   traditional (|q|+|r|+|s|)/2 for cube coordinates, but needs no division.
{-# INLINE ring #-}
ring :: (Integral a) => Cube a -> a
ring (Cube q r s) = max (abs q) (max (abs r) (abs s))

-- | Length of a hex vector
distance :: (Integral a) => Torsor a -> a
distance (Torsor dq dr ds) = max (abs dq) (max (abs dr) (abs ds))

-- | Floating point distance between two positions
distanceF :: (Floating a) => Torsor a -> a
distanceF (Torsor dq dr ds) = (sqrt $ dq*dq + dr*dr + ds*ds) / 2

-- | Origin hex at (0,0,0)
origo :: (Num a) => Cube a
origo = Cube 0 0 0

-- | Find closest cardinal direction between two positions
bearing :: (Num a, Ord a) => Cube a -> Cube a -> Torsor a
bearing p q = cardinal (facing (q <-> p))

-- | Closest of the six axial directions to a vector.  No lists, no folds,
--   just three differences and a few comparisons.
facing :: (Num a, Ord a) => Torsor a -> Direction
facing (Torsor dq dr ds)
  | absA >= absB && absA >= absC = if a >= 0 then NW else SE
  | absB >= absC                 = if b >= 0 then NE else SW
  | otherwise                    = if c >= 0 then  E else  W
  where
    a    = dr - ds
    b    = dr - dq
    c    = ds - dq
    absA = abs a
    absB = abs b
    absC = abs c


-- | Determine clockwise/counterclockwise relationship of vectors
orientation :: (Num a, Ord a) => Torsor a -> Torsor a -> a
orientation a b = signum $ case (a `cross` b) of
  (Torsor _ _ ds) -> ds

rotateCW :: (Num a) => Torsor a -> Torsor a
rotateCW (Torsor dq dr ds) = Torsor (-dr) (-ds) (-dq)

rotateCCW :: (Num a) => Torsor a -> Torsor a
rotateCCW (Torsor dq dr ds) = Torsor (-ds) (-dq) (-dr)




-- Convert cube coordinates to a spiral index
toSpiral :: (Integral a, Ord a) => Cube a -> a
toSpiral (Cube 0 0 0) = 0
toSpiral p = let   r = ring p
                   innerCells = r*(r-1)*3 + 1
                   (_ , (base, i)) = maximumBy (\x y -> compare (fst x) (fst y))
                                           $ ((\(idx,c) -> ((c <.> (p <-> origo)) , (c, idx)))
                                                 <$>  zip [0..5] cardinalsClockwise)
                   closestCorner = origo <+> scale r base
                   toClosestCorner = p <-> closestCorner
                   offsetFromCorner = orientation base toClosestCorner * distance toClosestCorner
                   offset = r+1
             in innerCells + (fromInteger i * r + offsetFromCorner - offset) `mod` (r*6)

radiusFromIndex :: (Integral a) => a -> a
radiusFromIndex index = search 0 index
  where
    formula r = 1 + 3 * r * (r - 1)
    search lo hi
      | lo >= hi  = lo
      | otherwise =
          let mid = (lo + hi) `div` 2
          in if formula mid >= index
             then search lo mid
             else search (mid + 1) hi

fromSpiral :: Integral a => a -> Cube a
fromSpiral 0 = cube 0 0 0
fromSpiral n = closestCorner <+> offsetFromCorner where
                r = radiusFromIndex (n + 1) -1
                innerCells = r*(r-1)*3 + 1
                offset = r + 1
                n' = (n - innerCells + offset) `mod` (r*6)
                (i,p) = n' `divMod` r
                base = cardinalsClockwise !! (fromIntegral i)
                closestCorner = origo <+> scale r base
                offsetFromCorner = scale p (rotateCW $ rotateCW $  base)


-- | Scale any functor containing numbers by a factor
scale :: (Functor f, Num a) => a -> f a -> f a
scale k = ((k*) <$>)

-- | Round a rational number to the nearest integer
roundDiff :: (Integral a) => Ratio a -> (a, Ratio a)
roundDiff a = let (n,d) = (numerator a, denominator a)
                  (i,r) = n `divMod` d
                  up = (i+1, (d-r) % d)
                  down = (i, r % d)
                  in case (compare (2*r) d , odd i) of
                      (LT, _)     -> down
                      (EQ, True)  -> up
                      (EQ, False) -> down
                      (GT, _)     -> up



-- | Test if two hexes share an edge
isAdjacent :: (Num a, Eq a) => Cube a -> Cube a -> Bool
isAdjacent (Cube q1 r1 s1) (Cube q2 r2 s2) =
  abs (q1 - q2) + abs (r1 - r2) + abs (s1 - s2) == 2

{-# INLINE nearestHex #-}
nearestHex :: (RealFrac a, Integral b) => Cube a -> Cube b
nearestHex (Cube !q !r !s) = let q' = round q
                                 r' = round r
                                 s' = round s
                               in snd $ maximumBy (compare `on` fst) [(abs (q-fromIntegral q'), Cube (-r'-s') r' s')
                                                                     ,(abs (r-fromIntegral r'), Cube q' (-q'-s') s')
                                                                     ,(abs (s-fromIntegral s'), Cube q' r' (-q'-r'))]



{-# INLINE nearestHexR #-}
nearestHexR :: (Integral a) => Cube (Ratio a) -> Cube a
nearestHexR (Cube !q !r !s) = let (q',dq) = roundDiff q
                                  (r',dr) = roundDiff r
                                  (s',ds) = roundDiff s
                               in snd $ maximumBy (compare `on` fst) [(dq, Cube (-r'-s') r' s')
                                                                     ,(dr, Cube q' (-q'-s') s')
                                                                     ,(ds, Cube q' r' (-q'-r'))]




-- 0 ≤ n < 6·r   – position of a cube on its ring, counted clockwise
spiralStep :: Integral a => a -> Cube a -> a
spiralStep !r (Cube !q !r' !s')
  | r' ==  r  =  -q                        -- edge 0  (N-W → N-E)
  | q  == -r  =  r  - r'    +   r          -- edge 1
  | s' ==  r  =  q  + r     + 2*r          -- edge 2
  | r' == -r  =  q          + 3*r          -- edge 3
  | q  ==  r  =  r' + r     + 4*r          -- edge 4
  | otherwise =  r'         + 5*r          -- edge 5  (s == -r)
{-# INLINE spiralStep #-}

-- key used only *within* a single ring, so we do no work for r == 0
spiralKey :: Integral a => a -> Cube a -> a
spiralKey !r !c = (spiralStep r c - (r+1)) `mod` (6*r)
{-# INLINE spiralKey #-}

instance (Integral a, Ord a) => Ord (Cube a) where
  {-# INLINE compare #-}
  compare a b =
        case compare ra rb of
          LT -> LT
          GT -> GT
          EQ | ra == 0  -> EQ                         -- both are (0,0,0)
             | otherwise -> compare (spiralKey ra a) (spiralKey rb b)
    where
      !ra = ring a
      !rb = ring b

instance (Integral a, Ord a) => Ord (Torsor a) where
  {-# INLINE compare #-}
  compare (Torsor a b c) (Torsor x y z) = compare (Cube a b c) (Cube x y z)

primitiveDirection :: Torsor Integer -> Torsor Integer
primitiveDirection (Torsor dq dr ds)
  | dq == 0 && dr == 0 && ds == 0
      = error "primitiveDirection: zero vector"
  | otherwise
      = let g = gcd (abs dq) $ gcd (abs dr) (abs ds)
        in  Torsor (dq `quot` g) (dr `quot` g) (ds `quot` g)
