#!/usr/bin/env stack
-- stack --resolver lts-12.20 script

import qualified Data.Set as Set

main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStrLn
        $ show
        . findDup Set.empty 
        . scanl (+) 0 
        . cycle 
        . map (read . stripPlus) 
        . lines 
        $ contents
    where
        stripPlus = filter (flip notElem "+")
        findDup acc (x:xs) =
            if Set.notMember x acc then
                findDup (Set.insert x acc) xs
            else
                x

