module Axial (
    Axial(..),
    spiral,
    rasterScan
) where

data Axial = Axial {q :: Int, r :: Int} deriving (Eq, Show)


axPlus :: Axial -> Axial -> Axial
axPlus a0 a1 = Axial ((q a0) + (q a1)) ((r a0) + (r a1))

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
corners :: [Axial] -> Int -> [Axial]
corners dirs r = map (scale r) dirs

neighbour :: Axial -> Axial -> Axial
neighbour origin d = Axial ((q origin) + (q d)) ((r origin) + (r d))

pathInDirection :: Int -> Axial -> Axial -> [Axial]
pathInDirection 0 _ _ = []
pathInDirection dist start dir = start : pathInDirection (dist-1) (neighbour start dir) dir

ringAround :: Axial -> Int -> [Axial]
ringAround origin radius = map (axPlus origin) (concat $ map pathFromTuple vectors)
    where vectors = zip (rotate 4 (corners directions radius)) directions
          pathFromTuple = uncurry (pathInDirection radius)

spiralAround :: Axial -> Int -> [Axial]
spiralAround origin 0 = [origin]
spiralAround origin radius = spiralAround origin (radius - 1) ++ ringAround origin radius

spiral :: Int -> [Axial]
spiral = spiralAround (Axial 0 0)

rasterScan :: Axial -> Int -> [[Axial]]
rasterScan origin radius = map hWalk firstTiles
    where firstTiles = take walkLen $ rotate rotation $ ringAround origin radius
          walkLen = (2 * radius + 1)
          rotation = 4 * radius
          hWalk a = pathInDirection (walkLen - abs(r a)) a (directionFromIndex 0)
