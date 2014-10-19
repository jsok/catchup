-- Coordinate systems uses in a hexagonal grid map
module Coordinates (
    Axial(..),
    ring
) where

import Data.List (maximum)

class Coordinate c where
  origin  :: c
  toAxial :: c -> Axial
  toCube  :: c -> Cube

data Axial = Axial {
        q :: Int
      , r :: Int
    } deriving (Eq, Show)

instance Coordinate Axial where
    origin = Axial 0 0
    toAxial c = c
    toCube c = Cube { x = x', y = y', z = z' }
        where x' = q c
              z' = r c
              y' = (-1) * x' - z'

data Cube = Cube {
        x :: Int
      , y :: Int
      , z :: Int
    } deriving (Eq, Show)

instance Coordinate Cube where
    origin = Cube 0 0 0
    toCube c = c
    toAxial c = Axial { q = x c, r = z c }

distance :: (Coordinate c) => c -> c -> Int
distance c1 c2 = maximum [x', y', z']
    where c1' = toCube c1
          c2' = toCube c2
          x' = abs ((x c1') - (x c2'))
          y' = abs ((y c1') - (y c2'))
          z' = abs ((z c1') - (z c2'))

distanceFromOrigin :: (Coordinate c) => c -> Int
distanceFromOrigin = distance origin

isValid :: (Coordinate c) => c -> Bool
isValid c = (x c') + (y c') + (z c') == 0
    where c' = toCube c

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

directions :: [Axial]
directions = [Axial 1 0, Axial 1 (-1), Axial 0 (-1), Axial (-1) 0, Axial (-1) 1, Axial 0 1]

directionFromIndex :: Int -> Axial
directionFromIndex i = directions !! (i `mod` 6)

scale :: Int -> Axial -> Axial
scale k a = Axial ((q a) * k) ((r a) * k)

-- Returns the coordinates of each corner of a hexgaon with given radius
corners :: Int -> [Axial]
corners r = map (scale r) directions

neighbour :: Axial -> Axial -> Axial
neighbour origin d = Axial ((q origin) + (q d)) ((r origin) + (r d))

pathInDirection :: Int -> Axial -> Axial -> [Axial]
pathInDirection 0 _ _ = []
pathInDirection dist start dir = start : pathInDirection (dist-1) (neighbour start dir) dir

ring :: Int -> [Axial]
ring radius = concat $ map pathFromTuple vectors
    where vectors = zip (rotate 4 (corners radius)) directions
          pathFromTuple = uncurry (pathInDirection radius)
