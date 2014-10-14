-- Coordinate systems uses in a hexagonal grid map
module Coordinates where

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
