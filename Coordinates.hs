-- Coordinate systems uses in a hexagonal grid map

data Axial = Axial {
        q :: Int
      , r :: Int
    } deriving (Eq, Show)

data Cube = Cube {
        x :: Int
      , y :: Int
      , z :: Int
    } deriving (Eq, Show)

axial2cube :: Axial ->  Cube
axial2cube a = Cube { x = q a, y = a_y, z = a_z }
    where a_x = q a
          a_z = r a
          a_y = (-1) * a_x - a_z

cube2axial :: Cube -> Axial
cube2axial c = Axial { q = x c, r = z c }
