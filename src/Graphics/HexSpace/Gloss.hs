module Graphics.HexSpace.Gloss where

import Data.HexSpace.Coordinates
import Data.HexSpace.Grid
import Graphics.Gloss
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map

grid :: (Integral i) => Grid i Picture -> Picture
grid = pictures . Foldable.toList
                . Map.mapWithKey hex

hex :: (Integral i) => Cube i -> Picture -> Picture
hex = pure translate <*> cubeX 
                     <*> cubeY
                     <$> (fromIntegral <$>)

hexF :: Cube Float -> Picture -> Picture
hexF = pure translate <*> cubeX 
                      <*> cubeY

wireHex :: Picture
wireHex = let corners = cartesianTorsor . unitHexCorner <$> [NC .. NWC] ++ [NC]
            in line corners

filledHex :: Picture
filledHex = let corners = cartesianTorsor . unitHexCorner <$> [NC .. NWC] ++ [NC]
            in polygon corners
