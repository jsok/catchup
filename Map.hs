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

import Data.Either
import Data.Array

import Coordinates


data Player = Player1 | Player2 deriving (Eq, Show)

data Tile = Tile {
      claimed :: Maybe Player
    }
    deriving (Eq, Show)

type Rows = Array (Int, Int) (Maybe Tile)
data Board = Board {
        rows :: Rows
      , radius :: Int
    }
    deriving (Show)

-- Creates a hexagon point-topped board of width `n`
board :: Int -> Board
board 0 = error "Map size must be at least 1"
board width = Board rows radius
    where radius = width - 1
          -- e.g. width 2 has 3 rows (2, 3, 2), 0-based array
          boardBounds = ((width * 2) - 1) - 1
          ranges = ((0,0), (boardBounds, boardBounds))
          rows = array ranges [(c, Just (Tile Nothing)) | c <- range ranges]

-- Calculates the array index of a coordinate in hexagon shaped map with radius N
axialToIndex :: Axial -> Int -> (Int, Int)
axialToIndex a n = ((r a) + n, (q a) + n + (min 0 (r a)))

indexToAxial :: (Int, Int) -> Int -> Maybe Axial
indexToAxial (x,y) n = maybeAxial (q,r) n
    where r = x - n
          q = y - n - (min 0 r)

maybeAxial :: (Int,Int) -> Int -> Maybe Axial
maybeAxial (q,r) n = if distanceFromOrigin c <= n
                     then Just a
                     else Nothing
    where a = Axial q r
          c = toCube a


tileAtIndex :: Board -> (Int, Int) -> Maybe Tile
tileAtIndex b i = (rows b)!i

updateBoard :: Board -> (Int, Int) -> Maybe Tile -> Board
updateBoard b idx t = Board ((rows b)//[(idx, t)]) (radius b)

claimTileOnBoard :: Board -> (Int, Int) -> Player -> Either String Board
claimTileOnBoard b idx p =
    case tileAtIndex b idx of
        Nothing -> Left ("No tile at given index " ++ show idx)
        Just t -> case claimTile t p of
            Right t' -> Right (updateBoard b idx (Just t'))
            Left s -> Left s

claimTile :: Tile -> Player -> Either String Tile
claimTile (Tile {claimed = Nothing}) p = Right (Tile (Just p))
claimTile (Tile {claimed = Just c}) p = if c == p
                                        then Left (show c ++ " cannot claim tile again")
                                        else Left ("Tile claimed by " ++ show c)
