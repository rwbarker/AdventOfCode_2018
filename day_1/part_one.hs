#!/usr/bin/env stack
-- stack --resolver lts-12.20 script

main :: IO ()
main = do
    contents <- readFile "input.txt"
    putStrLn $ show . sum . map read . (map stripPlus) . lines $ contents
    where
        stripPlus :: [Char] -> [Char]
        stripPlus = filter (flip notElem "+")