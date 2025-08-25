# HexSpace

A Haskell library for hex-based grids, providing data structures and algorithms for working with hexagonal coordinate systems.

## Overview

HexSpace implements hexagonal grids using cube coordinates, aiming at efficient operations for hex-based games and applications.

Some functions work with `Integral` coordinate types, while others work with `Fractonal`.

## Features

### Coordinate Systems

- **Cube coordinates**: The primary coordinate system using (q, r, s) where q + r + s = 0 (any `Num` type)
- **Axial coordinates**: Simplified (x, y) representation (any `Num` type)
- **Cartesian conversion**: Transform between hex and screen coordinates (requires `Floating` types)
- **Spiral indexing**: Convert between coordinates and spiral indices (requires `Integral` types)

### Grid Operations
- **Distance calculations**: Hexagonal distance and floating-point distance (`Floating`)
- **Direction vectors**: Six cardinal directions (NW, NE, E, SE, SW, W) (any `Num` type)
- **Adjacency testing**: Check if hexes share an edge (any `Num` type with `Eq`)
- **Ring operations**: Find hexes at a specific distance from origin (`Integral` for efficient ring calculation)
- **Rotation**: Clockwise and counterclockwise rotation of vectors (any `Num` type)

### Graphics Integration
- **Gloss rendering**: Render hex grids using the Gloss graphics library (uses `Float` internally)
- **Hex shapes**: Pre-defined wireframe and filled hexagon shapes
- **Grid visualization**: Convert hex grids to renderable pictures

## Quick Start

```haskell
import Data.HexSpace

-- Create hex coordinates (works with any Num type)
let origin = origo :: Cube Int       -- (0,0,0) 
let hex1 = axial 2 1 :: Cube Int    -- Convert from axial coordinates
let hex2 = cube 1 (-1) 0 :: Cube Int -- Projects to the x + y + z = 0 plane.

-- Calculate distance (Integral types for exact distance)
let dist = distance (hex2 <-> hex1)  -- Manhattan distance

-- Find adjacent hexes (works with any Num type)
let neighbor = hex1 <+> cardinal NE  -- Move northeast

-- Pathfinding (requires Integral coordinates)
let walkableHexes = Set.fromList [origin, hex1, hex2] :: Set (Cube Int)
let path = findPath walkableHexes origin hex2

-- Screen coordinates (requires Floating types)
let screenPos = cartesian (cube 1.5 (-0.5) (-1.0) :: Cube Float)
let hexFromScreen = fromCartesian (100.0, 50.0) :: Cube Float
```

## Type-Specific Functions

### Integral Types (Int, Integer)
- `ring :: Integral a => Cube a -> a` - Fast distance to origin
- `distance :: Integral a => Torsor a -> a` - Manhattan distance  
- `toSpiral :: Integral a => Cube a -> a` - Convert to spiral index
- `fromSpiral :: Integral a => a -> Cube a` - Convert from spiral index
- `findPath :: Integral a => Set (Cube a) -> Cube a -> Cube a -> Maybe [Direction]`

### Fractional/Floating Types (Float, Double, Rational)
- `cubeX, cubeY :: Fractional a => Cube a -> a` - Screen X/Y coordinates
- `cartesian :: Floating a => Cube a -> (a, a)` - Convert to screen coordinates
- `fromCartesian :: Floating a => (a, a) -> Cube a` - Convert from screen coordinates
- `distanceF :: Floating a => Torsor a -> a` - Euclidean distance
- `nearestHex :: RealFrac a => Cube a -> Cube Int` - Round to nearest integer hex

### Polymorphic Functions (any Num type)
- `cube, axial, torsor` - Create coordinates
- `(<+>), (<->), (<.>)` - Vector operations  
- `cardinal, rotateCW, rotateCCW` - Direction operations
- `isAdjacent, facing, bearing` - Spatial relationships

## Modules

- `Data.HexSpace`: Main module re-exporting core functionality
- `Data.HexSpace.Coordinates`: Coordinate systems and transformations
- `Data.HexSpace.Grid`: Grid operations and pathfinding
- `Data.HexSpace.Line`: Line operations (internal)
- `Graphics.HexSpace.Gloss`: Gloss graphics integration

## Dependencies

- `base`: Standard Haskell base library
- `containers`: For Map and Set data structures
- `gloss`: Graphics rendering
- `PSQueue`: Priority queue for pathfinding
- `QuickCheck`: Property testing support

## License

BSD-3-Clause

## Author

Håkon Robbestad Gylterud
