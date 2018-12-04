#!/usr/bin/env stack
-- stack --resolver lts-12.20 script

import Data.List.Utils (replace)
import Data.List (any)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStrLn
        $ show
        $ intactClaims
        $ map parseClaim
        $ lines
        $ contents
    where 
        getCoords (claim, coordPair) = coordPair
        otherClaims x = filter (/=x)
        intactClaims xs = 
            filter (\x -> not (overlapsAny (getCoords x) (map getCoords (otherClaims x xs)))) xs

        
overlapsAny :: ((Int,Int),(Int,Int)) -> [((Int,Int),(Int,Int))] -> Bool
overlapsAny coord otherCoords = 
    any (overlaps coord) otherCoords

overlaps :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int)) -> Bool
overlaps ((x,y),(a,b)) ((x',y'),(a',b')) =
    not $ x > (x' + a' - 1) || (x + a - 1) < x' || y > (y' + b' - 1) || (y + b - 1) < y'

parseClaim :: String -> (String, ((Int,Int),(Int,Int)))
parseClaim line =
    (claimStr, (positionCoord,sizeCoord))
    where
        readCoord :: String -> (Int,Int)
        readCoord s = read s
        parts = words line
        positionStr = "(" ++ init (parts !! 2) ++ ")"
        sizeStr = "(" ++ replace "x" "," (parts !! 3) ++ ")"
        claimStr = replace "#" "" (parts !! 0)
        positionCoord = readCoord positionStr
        sizeCoord = readCoord sizeStr
        --createCoords (x,y) (a,b) = [(posX,posY) | posY <- [y..(y+b-1)], posX <- [x..(x+a-1)]]