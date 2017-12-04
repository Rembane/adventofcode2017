module Main where

import Data.List

solveOne :: IO ()
solveOne =
  print =<< length . filter null . map (filter (>1) . map length . group . sort . words) . lines <$> readFile "input.txt"

solveTwo :: IO ()
solveTwo =
  print =<< length . filter null . map (filter (>1) . map length . group . sort . map sort . words) . lines <$> readFile "input.txt"

main :: IO ()
main = solveOne >> solveTwo

