#!/usr/bin/env stack
-- stack --resolver lts-12.20 script

import Data.List (intersect)
import Data.List.Utils (replace)
import qualified Data.Map as Map

main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStrLn
        $ show
        $ length
        $ filter (\(pos,count) -> count >= 2)
        $ frequencies
        $ concat
        $ map parseClaim
        $ lines
        $ contents
    where 
        frequencies xs = Map.toList $ Map.fromListWith (+) [(x,1) | x <- xs]
                
parseClaim :: String -> [(Int,Int)]
parseClaim line =
    createCoords positionCoord sizeCoord
    where
        readCoord :: String -> (Int,Int)
        readCoord s = read s
        parts = words line
        positionStr = "(" ++ init (parts !! 2) ++ ")"
        sizeStr = "(" ++ replace "x" "," (parts !! 3) ++ ")"
        positionCoord = readCoord positionStr
        sizeCoord = readCoord sizeStr
        createCoords (x,y) (a,b) = [(posX,posY) | posY <- [y..(y+b-1)], posX <- [x..(x+a-1)]]