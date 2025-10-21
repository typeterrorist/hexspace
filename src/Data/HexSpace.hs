

module Data.HexSpace ( Cube
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
  , opposite
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
  , Line
  , findPath
  ) where

import  Data.HexSpace.Coordinates

import Data.HexSpace.Line (Line)
import Data.HexSpace.Grid (findPath)
