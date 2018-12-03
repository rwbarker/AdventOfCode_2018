#!/usr/bin/env stack
-- stack --resolver lts-12.20 script

import Data.List (sortBy)
import Data.Function (on)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStrLn
        $ head
        $ reverse
        $ sortBy stringLength
        $ findSimilarIds
        $ lines
        $ contents
        where
            stringLength = compare `on` length
            matchingCharacters as bs = 
                map fst
                $ filter (\(a,b) -> a == b)
                $ zip as bs
            findSimilarIds xs = findSimilarIds' [] (head xs) (tail xs)
            findSimilarIds' acc x xs = 
                if length xs == 1 then
                    concat ((map (matchingCharacters x) xs):acc)
                else
                    findSimilarIds' ((map (matchingCharacters x) xs):acc) (head xs) (tail xs)
                    
                


