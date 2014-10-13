-- Coordinate systems uses in a hexagonal grid map
module Coordinates (
    Axial, Cube,
    axial2cube, cube2axial,
    cubeDistanceFromOrigin
) where

data Axial = Axial {
        q :: Int
      , r :: Int
    } deriving (Eq, Show)

data Cube = Cube {
        x :: Int
      , y :: Int
      , z :: Int
    } deriving (Eq, Show)


isCubeValid :: Cube -> Bool
isCubeValid c = (x c) + (y c) + (z c) == 0

axial2cube :: Axial ->  Cube
axial2cube a = Cube { x = a_x, y = a_y, z = a_z }
    where a_x = q a
          a_z = r a
          a_y = ((-1) * a_x) - a_z

cube2axial :: Cube -> Axial
cube2axial c = Axial { q = x c, r = z c }

cubeDistance :: Cube -> Cube -> Int
cubeDistance c1 c2 = maximum [x', y', z']
    where x' = abs ((x c1) - (x c2))
          y' = abs ((y c1) - (y c2))
          z' = abs ((z c1) - (z c2))

cubeDistanceFromOrigin = cubeDistance (Cube 0 0 0)
