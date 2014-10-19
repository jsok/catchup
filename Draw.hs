--
--        / \     / \
--      /     \ /     \
--     | -1,-1 |  1,-1 |
--     |   -R  |       |
--    / \     / \     / \
--  /     \ /     \ /     \
-- | -1,0  |  0,0  |  1,0  |
-- |  -Q   |       |   +Q  |
--  \     / \     / \     /
--    \ /     \ /     \ /
--     | -1,1  |  0,1  |
--     |       |   +R  |
--      \     / \     /
--        \ /     \ /

module Draw (
    drawBoard
) where

import Data.List (intersperse)

import Axial
import Board


spaces = "    "
peak1 = "   / \\  "
peak2 = " /     \\"
-- cell2 = "|       "
trough1 = " \\     /"
trough2 = "   \\ /  "

cell1 :: Axial -> String
cell1 a = "|" ++ coord
    where coord = lpad ++ show (q a) ++ "," ++ show (r a) ++ rpad
          lpad = take (3 - length (show (q a))) $ iterate id ' '
          rpad = take (3 - length (show (r a))) $ iterate id ' '

cell2 :: Tile -> String
cell2 t = "|  " ++ who (claimedBy t) ++ "   "
    where who (Just Player1) = "P1"
          who (Just Player2) = "P2"
          who Nothing = "  "

drawN :: Int -> String -> String
drawN n x = concat $ take n $ iterate id x

drawRow :: Board -> Int -> [Axial] -> [String]
drawRow b radius tiles@(start:_) = peak ++ cell ++ trough
    where maxWidth = (2 * radius) + 1
          width = (maxWidth - abs(r start))
          tileAt a = tileAtIndex b (axialToIndex radius a)
          padding = drawN (maxWidth - width) spaces
          peak = [padding ++ drawN width peak1, padding ++ drawN width peak2]
          cell = [padding ++ (concat $ map cell1 tiles) ++ "|",
                  padding ++ (concat $ map (cell2 . tileAt) tiles) ++ "|"]
          trough = [padding ++ drawN width trough1, padding ++ drawN width trough2]

drawBoard :: Board -> [String]
drawBoard b = concat $ map (drawRow b n) (rasterScan origin n)
    where n = radius b
          origin = Axial 0 0
