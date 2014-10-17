-- Coordinate systems uses in a hexagonal grid map
module Coordinates (
    Coordinate(..),
    Axial(..),
    Cube(..),
    distance, distanceFromOrigin,
    isValid
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


directions :: [Axial]
directions = [Axial 1 0, Axial 1 (-1), Axial 0 (-1), Axial (-1) 0, Axial (-1) 1, Axial 0 1]

direction :: Int -> Axial
direction i = directions !! (i `mod` 6)

scale :: Int -> Axial -> Axial
scale k a = Axial ((q a) * k) ((r a) * k)

-- neighbour :: Axial -> Int -> Axial
-- neighbour origin i = Axial ((q origin) + (q d)) ((r origin) + (r d))
--     where d = directions !! (i `mod` 6)

neighbour :: Axial -> Axial -> Axial
neighbour origin d = Axial ((q origin) + (q d)) ((r origin) + (r d))


-- TODO: zip starting point (each corner) with direction and map to pathInDirection
-- ring :: Axial -> Int -> [Axial]
-- ring origin radius = concat $ map (walkBorder h radius) [0..5]
--     where h = scale (direction 4) radius
--           border i = take radius $ iterate ((flip neighbour) i) (h i)

walkInDirectionTimes :: Axial -> Int -> Int -> [Axial]
walkInDirectionTimes h dir 0 = []
walkInDirectionTimes h dir r = h : walkInDirectionTimes (neighbour h dir) dir (r-1)

pathInDirection :: Axial -> Int -> Axial -> [Axial]
pathInDirection _ 0 _ = []
pathInDirection dir r start = start : pathInDirection dir (r-1) (neighbour start dir)
