{-# LANGUAGE ScopedTypeVariables #-}
module Data.HexSpaceSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.HexSpace
import qualified Data.HexSpace.Line as Line

import Data.List (nub)

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Sequence as Seq
import Control.Applicative.Logic (convert)

spec :: Spec
spec = do
  describe "Spiral coordinates" $ do
    it "successive spiral coordinates are adjacent" $ property $
      \(NonNegative (n :: Integer)) -> isAdjacent (fromSpiral n) (fromSpiral (n + 1))

    it "toSpiral and fromSpiral are inverses" $ property $
      \(n :: Integer) -> n >= 0 ==> toSpiral (fromSpiral n) === n

    it "fromSpiral and toSpiral are inverses" $ property $
      \(c :: Cube Integer) -> fromSpiral (toSpiral c) === c

    it "spiral origin is at (0,0,0)" $
      fromSpiral 0 `shouldBe` origo

  describe "Ordering and spiral coordinates" $ do
    it "preserves ordering between spiral indices and cubes" $ property $
      \(NonNegative (n1 :: Integer)) (NonNegative (n2 :: Integer)) ->
        compare n1 n2 === compare (fromSpiral n1) (fromSpiral n2)

    it "maintains strict ordering" $ property $
      \(NonNegative (n1 :: Integer)) (NonNegative (n2 :: Integer)) ->
        n1 < n2 ==> fromSpiral n1 < fromSpiral n2

    it "is consistent with toSpiral" $ property $
      \(c1 :: Cube Integer) (c2 :: Cube Integer) ->
        compare (toSpiral c1) (toSpiral c2) === compare c1 c2

  describe "Ring and distance functions" $ do
    it "ring of origin is 0" $
      ring origo `shouldBe` 0

    it "ring increases with distance from origin" $ property $
      \(c :: Cube Integer) -> ring c >= 0

    it "distance between adjacent hexes is 1" $ property $
      \(c :: Cube Integer) -> all (\dir -> distance dir == 1) cardinalsClockwise

    it "distance is symmetric" $ property $
      \(t1 :: Torsor Integer) (t2 :: Torsor Integer) -> 
        distance (t1 + t2) == distance (t2 + t1)

  describe "Vector operations" $ do
    it "vector addition is commutative" $ property $
      \(t1 :: Torsor Integer) (t2 :: Torsor Integer) -> 
        t1 + t2 === t2 + t1

    it "moving by a vector and back reaches start" $ property $
      \(c :: Cube Integer) (t :: Torsor Integer) ->
        (c <+> t) <+> (Data.HexSpace.scale (-1) t) === c

    it "vector between points works with addition" $ property $
      \(c1 :: Cube Integer) (c2 :: Cube Integer) ->
        c1 <+> (c2 <-> c1) === c2

    it "dot product with self is non-negative" $ property $
      \(t :: Torsor Integer) -> t <.> t >= 0

    it "cross product is perpendicular" $ property $
      \(t1 :: Torsor Integer) (t2 :: Torsor Integer) ->
        (t1 `cross` t2) <.> t1 === 0

    it "rotateCW moves through cardinalsClockwise in clockwise order" $ do
      (d:_) <- pure cardinalsClockwise
      let rotated = take 6 $ iterate rotateCW d
      rotated `shouldBe` cardinalsClockwise

    it "rotateCCW moves through cardinalsClockwise in counter-clockwise order" $
      let rotated = take 6 $ iterate rotateCCW (last cardinalsClockwise)
      in rotated `shouldBe` reverse cardinalsClockwise

    it "rotateCW and rotateCCW are inverses" $ property $
      \(t :: Torsor Integer) -> rotateCW (rotateCCW t) === t

    it "six rotations clockwise equals identity" $ property $
      \(t :: Torsor Integer) -> 
        (rotateCW . rotateCW . rotateCW . rotateCW . rotateCW . rotateCW) t === t
    it "torsor is one third the sum of its axial components" $ property $
      \(t :: Torsor Integer) -> Data.HexSpace.scale 3 t === Data.HexSpace.scale (component t NW) (cardinal NW)
                                                       + Data.HexSpace.scale (component t E) (cardinal E)
                                                       + Data.HexSpace.scale (component t SW) (cardinal SW)
    it "Components are dot products with cardinals" $ property $
     \(t :: Torsor Integer) (d :: Direction) -> component t d === t <.> cardinal d


  describe "Adjacent hexes" $ do
    it "cardinal directions are adjacent to origin" $
      all (isAdjacent origo . (origo <+>)) cardinalsClockwise

    it "hexes at distance more than 2 are not adjacent" $ property $
      \(c1 :: Cube Integer) (c2 :: Cube Integer) -> abs (ring c1 - ring c2) >= 2 ==> not (isAdjacent c1 c2)

    it "adjacency is symmetric" $ property $
      \(c1 :: Cube Integer) (c2 :: Cube Integer) ->
        isAdjacent c1 c2 === isAdjacent c2 c1
    it "adjacent hexes are at distance 1" $ property $
      \(c1 :: Cube Integer) (c2 :: Cube Integer) ->
        isAdjacent c1 c2 === (distance (c1 <-> c2) == 1)
  
  describe "Rounding" $ do
    it "RoundDiff gives the rounding and the difference" $ property $
      \(f :: Rational) -> roundDiff f == (round f, abs (f - fromIntegral (round f)))
    it "Rounding is the nearest hex" $ property $
      \(c :: Cube Float) (h :: Cube Integer) -> distanceF (c <-> (fromInteger <$> nearestHex c)) <= distanceF (c <-> (fromInteger <$> h))
    it "nearestHex is nearestHexR" $ property $
      \(c :: Cube Rational) -> nearestHex c === nearestHexR c
  describe "Line drawing" $ do
    it "line starts at the beginning" $ property $
       \(l :: Line Integer) -> head (Line.hexes l) === Line.start l
    it "line ends at the end" $ property $
       \(l :: Line Integer)  -> last (Line.hexes l) === Line.end l
    it "line gives adjacent hexes" $ property $
       \(l :: Line Integer)  -> all (uncurry isAdjacent) (zip (Line.hexes l) (tail (Line.hexes l)))
    it "line does not repeat hexes" $ property $
       \(l :: Line Integer)  -> nub (Line.hexes l) === Line.hexes l
    it "Beam intersection is in first beam" $ property $
       \(b1 :: Line.Beam Rational) (b2 :: Line.Beam Rational) -> case Line.intersectBeam b1 b2 of
              (Just (p,t,u)) -> p === Line.origin b1 <+> (Data.HexSpace.scale t (Line.direction b1))
              Nothing -> 1 === 1
    it "Beam intersection is in the second beam" $ property $
       \(b1 :: Line.Beam Rational) (b2 :: Line.Beam Rational) -> case Line.intersectBeam b1 b2 of
              (Just (p,t,u)) -> p === Line.origin b2 <+> (Data.HexSpace.scale u (Line.direction b2))
              Nothing -> 1 === 1
  describe "Direction and orientation functions" $ do
    it "facing returns the closest cardinal direction" $ property $
      \(t :: Torsor Integer) ->
        let d = facing t
            c = cardinal d
        in all (\d' -> t <.> c >= t <.> cardinal d') [NW .. W]

    it "bearing returns a cardinal direction vector" $ property $
      \(c1 :: Cube Integer) (c2 :: Cube Integer) ->
        bearing c1 c2 `elem` cardinalsClockwise

    it "bearing is consistent with facing" $ property $
      \(c1 :: Cube Integer) (c2 :: Cube Integer) ->
        facing (c2 <-> c1) == facing (bearing c1 c2)

    it "orientation is positive for counterclockwise turns" $ property $
      \(d :: Direction) ->
        let d' = case d of
              W -> NW
              _ -> succ d
        in orientation (cardinal d) (cardinal d') > 0

    it "orientation is negative for clockwise turns" $ property $
      \(d :: Direction) ->
        let d' = case d of
              NW -> W
              _ -> pred d
        in orientation (cardinal d) (cardinal d') < 0

    it "orientation is zero for parallel vectors" $ property $
      \(t :: Torsor Integer) -> orientation t t == 0

    it "orientation is antisymmetric" $ property $
      \(t1 :: Torsor Integer) (t2 :: Torsor Integer) ->
        orientation t1 t2 == negate (orientation t2 t1)
  
  describe "Facing" $ do
    it "facing of a cardinal direction is that direction" $ property $
      \(d :: Direction) ->
        facing (cardinal d) === d

    it "facing gives direction with maximal dot product" $ property $
      \(t :: Torsor Integer) ->
        let d = facing t
            c = cardinal d
        in all (\d' -> (c <.> t) >= (cardinal d' <.> t)) [minBound .. maxBound]

    it "zero vector faces NW" $
      facing (torsor 0 0 0) `shouldBe` NW

  describe "findPath" $ do
    it "findPath returns Nothing when start or goal is not in walkable set" $ property $
      \(start :: Cube Integer) (goal :: Cube Integer) (xs :: [Cube Integer]) ->
        let walkable = Set.fromList xs
        in start /= goal && (not (Set.member start walkable) || not (Set.member goal walkable)) ==>
            findPath walkable start goal === Nothing
    it "findPath from a cube to itself is empty path" $ property $
      \(start :: Cube Integer) -> 
        let walkable = Set.singleton start
        in findPath walkable start start === Just []

    it "findPath returns a walkable path from start to goal" $ property $
      \(start :: Cube Integer) (NonEmpty xs :: NonEmptyList Direction) ->
        let path     = take 10 xs
            visited  = scanl (<+>) start (map cardinal path)
            walkable = Set.fromList visited
            goal     = last visited
        in start /= goal ==>                  -- avoid the trivial cycle case
             case findPath walkable start goal of
               Nothing   -> property False    -- a path *must* exist
               Just p    ->
                 let positions = scanl (<+>) start (map cardinal p)
                 in conjoin
                      [ last positions === goal                   -- reaches goal
                      , property $ all (`Set.member` walkable) positions     -- never leaves walkable
                      , property $ all (uncurry isAdjacent)                  -- moves one step at a time
                            (zip positions (tail positions))
                      , property $ length p <= length path                   -- no longer than the fixture path
                      ]


  it "produces only walkable steps" $ property $
    \(start :: Cube Integer) (xs :: [Direction]) ->
      let path = take 10 xs
          visited = scanl (<+>) start (map cardinal path)
          walkable = Set.fromList visited
          goal = last visited
      in case findPath walkable start goal of
          Just result -> property $ all (`Set.member` walkable) (scanl (<+>) start (map cardinal result))
          Nothing     -> property True  -- allowed

  it "findPath never leaves walkable region" $ property $
    \(start :: Cube Integer) (xs :: [Direction]) ->
      let path = take 10 xs
          visited = scanl (<+>) start (map cardinal path)
          walkable = Set.fromList visited
          goal = last visited
      in case findPath walkable start goal of
          Just result -> property $ all (`Set.member` walkable) (scanl (<+>) start (map cardinal result))
          Nothing     -> property True

   -- (1) Optimality: length == true shortest distance on a full ball.
  it "returns a path whose length equals cube distance on an obstacle-free ball"
    $ forAll genSmallConnected $ \(w,start,goal) ->
        start /= goal ==>
          fmap length (findPath w start goal)
            === Just (distance (goal <-> start))

  -- (2) Agreement with BFS (both success and failure) on arbitrary *small*
  --     walkable subsets.
  it "agrees with a reference BFS on small instances" $
    forAll genSmallConnected $ \(full,start,goal) ->
      forAll
        -- choose any *subset* of the full ball that still contains both endpoints
        (sublistOf (Set.toList full) `suchThat`
          \xs -> start `elem` xs && goal `elem` xs)
        $ \xs ->
          let s    = Set.fromList xs          -- now the Set is in scope
              bfs  = bfsDist s start goal
              ours = fmap length (findPath s start goal)
          in maybe (ours === Nothing) (\d -> ours === Just d) bfs


  -- (3) Symmetry: swapping start/goal does not change the length.
  it "produces symmetric path lengths" $
    forAll genSmallConnected $ \(w,s,g) ->
      let l f x y = fmap length (f w x y)
      in l findPath s g === l findPath g s

  -- (4) Monotonicity: enlarging the walkable set never lengthens the path.
  it "is monotone under addition of walkable cells" $
    forAll genSmallConnected $ \(w0,start,goal) -> forAll
      (sublistOf (Set.toList w0)) $ \extra ->
        let w1   = w0
            w0'  = w0 Set.\\ Set.fromList extra
            lenW s = fmap length (findPath s start goal)
        in lenW w0' /= Nothing ==>
              lenW w1 <= lenW w0'


-- | All cubes in the closed ball of radius r, centred at the origin.
ball :: (Integral i, Ord i) => i -> Set (Cube i)
ball r = Set.fromList
  [ fromSpiral n | n <- [0 .. r * (r + 1) * 3] ]

-- | Vanilla BFS – good enough for r ≤ 3 ( ≤ 37 nodes ).
bfsDist :: forall i. (Integral i, Ord i)
        => Set (Cube i) -> Cube i -> Cube i -> Maybe Int
bfsDist walkable start goal = go (Seq.singleton start)
                                 (Map.singleton start 0)
  where
    go Seq.Empty _                 = Nothing
    go (p Seq.:<| q) dist
      | p == goal                  = Just (dist Map.! p)
      | otherwise                  =
          let nbs   = filter (`Set.member` walkable)
                     [ p <+> cardinal d | d <- [NW .. W] ]
              front = filter (`Map.notMember` dist) nbs
              dist' = foldr (\v -> Map.insert v (succ (dist Map.! p))) dist front
          in go (q Seq.>< Seq.fromList front) dist'

-- | Generate a *small* ball of radius ≤ 3 (≤ 37 cells) plus two distinct
--   points inside it.
genSmallConnected :: Gen (Set (Cube Int), Cube Int, Cube Int)
genSmallConnected = do
  Positive r <- (arbitrary :: Gen (Positive Int)) `suchThat` (\(Positive x)->x<=3)
  let w = ball r
  s <- elements (Set.toList w)
  g <- elements (Set.toList (Set.delete s w))
  pure (w,s,g)

