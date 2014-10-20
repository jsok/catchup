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


padding = "    "
peak1 = "   / \\  "
peak2 = " /     \\"
trough1 = " \\     /"
trough2 = "   \\ /  "

cell1 :: Axial -> String
cell1 a = "|" ++ coord
    where coord = lpad ++ show (q a) ++ "," ++ show (r a) ++ rpad
          pad n = take (3 - n) $ iterate id ' '
          lpad = pad (length (show (q a)))
          rpad = pad (length (show (r a)))

cell2 :: Tile -> String
cell2 t = "|  " ++ who (claimedBy t) ++ "   "
    where who (Just p) = show p
          who Nothing = "  "

drawN :: Int -> String -> String
drawN n x = concat $ take n $ iterate id x

drawRow :: Board -> [Axial] -> [String]
drawRow b tiles
        | rowIndex == 0 = peak ++ cell ++ trough
        | rowIndex < 0  = peak ++ cell
        | otherwise = cell ++ trough
    where rowIndex = r (head tiles)
          width = rowWidth b rowIndex
          indent = drawN ((maxRowWidth b) - width) padding
          peak = [indent ++ drawN width peak1,
                  indent ++ drawN width peak2]
          cell = [indent ++ (concatMap cell1 tiles) ++ "|",
                  indent ++ (concatMap (cell2 . tileAtAxial b) tiles) ++ "|"]
          trough = [indent ++ drawN width trough1,
                    indent ++ drawN width trough2]

drawBoard :: Board -> [String]
drawBoard b = concatMap (drawRow b) (rasterScan origin n)
    where n = radius b
          origin = Axial 0 0
