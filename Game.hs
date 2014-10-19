import Axial
import Board
import Draw

draw :: Board -> IO ()
draw b = putStrLn $ unlines $ drawBoard b

justRight :: Either a b -> b
justRight (Right b) = b
justRight (Left _) = error ""
