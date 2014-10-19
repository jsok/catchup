module Board (
    Tile(..),
    Board(..), board, claim,
    Player(..),
    axialToIndex, tileAtIndex
) where

import Data.Array.IArray
import Axial

data Player = Player1 | Player2 deriving (Eq, Show)

data Tile = Tile {
    claimedBy :: Maybe Player
} deriving (Show)

data Board = Board {
    tiles :: Array (Int, Int) Tile
  , radius :: Int
} deriving (Show)

axialToIndex :: Int -> Axial -> (Int, Int)
axialToIndex n a = ((r a) + n, (q a) + n + (min 0 (r a)))

board :: Int -> Board
board 0 = error "Map size must be at least 1"
board width = Board tiles radius
    where radius = width - 1
          -- e.g. width 2 has 3 rows (2, 3, 2), 0-based array
          bounds = ((width * 2) - 1) - 1
          els = [(i, Tile Nothing) | i <- map (axialToIndex radius) (spiral radius)]
          tiles = array ((0,0), (bounds,bounds)) els

updateBoard :: Board -> (Int, Int) -> Tile -> Board
updateBoard b idx t = Board ((tiles b)//[(idx, t)]) (radius b)

tileAtIndex :: Board -> (Int, Int) -> Tile
tileAtIndex b i = (tiles b)!i

claimTile :: Tile -> Player -> Either String Tile
claimTile (Tile {claimedBy = Nothing}) p = Right (Tile (Just p))
claimTile (Tile {claimedBy = Just c}) _ = Left ("Tile has already been claimed by " ++ show c)

claim :: Board -> Axial -> Player -> Either String Board
claim b a p = case claimTile tile p of
                Left s -> Left s
                Right t -> Right (updateBoard b idx t)
    where idx = axialToIndex (radius b) a
          tile = tileAtIndex b idx
