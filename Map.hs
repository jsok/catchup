-- Implements a hexagonal grid using an Axial co-ordinate system
--
--
--          / \   / \
--         /   \ /   \
--        |0,-1 |-1,-1|
--       / \   / \   /
--      /   \ /   \ /
--     |-1,0 | 0,0 |
--      \   / \   /
--       \ /   \ /

import Data.Array
import Data.List (permutations, maximum)

data Tile = Tile {
        coord :: Axial
      , played :: Bool
    }
    deriving (Eq, Show)

type Rows = Array (Int, Int) (Maybe Tile)
data Board = Board {
        rows :: Rows
      , sideWidth:: Int
      , radius :: Int
    }
    deriving (Show)

-- Creates a hexagon point-topped board of width `n`
board :: Int -> Board
board 0 = error "Map size must be at least 1"
board width = Board rows width radius
    where radius = width - 1
          -- e.g. width 2 has 3 rows (2, 3, 2), 0-based array
          boardBounds = ((width * 2) - 1) - 1
          ranges = ((0,0), (boardBounds, boardBounds))
          tile Nothing = Nothing
          tile (Just c) = Just (Tile c False)
          rows = array ranges [(c, tile (indexToAxial c radius)) | c <- range ranges]

-- Calculates the array index of a coordinate in hexagon shaped map with radius N
arrayIndex :: Axial -> Int -> (Int, Int)
arrayIndex a n = ((r a) + n, (q a) + n + (min 0 (r a)))

-- Calculates the Axial coordinate of the Tile at the given array index
indexToAxial :: (Int, Int) -> Int -> Maybe Axial
indexToAxial (x,y) n = maybeAxial (q,r) n
    where r = x - n
          q = y - n - (min 0 r)
