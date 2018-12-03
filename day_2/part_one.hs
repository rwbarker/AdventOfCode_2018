#!/usr/bin/env stack
-- stack --resolver lts-12.20 script

import qualified Data.Map as Map
import Data.List (nub, sort)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStrLn
        $ show
        $ inventoryChecksum
        $ unzip
        $ map lineChecksum
        $ map frequencies
        $ lines
        $ contents
    where
        frequencies xs = Map.toList $ Map.fromListWith (+) [(x,1) | x <- xs]
        lineChecksum xs = 
            case (nub $ sort $ map snd $ filter (\(x,ct) -> ct == 2 || ct == 3) xs) of
                [2,3]     -> (1,1)
                [2]       -> (1,0)
                [3]       -> (0,1)
                otherwise -> (0,0)
        inventoryChecksum (twos,threes) = (sum twos) * (sum threes)
                