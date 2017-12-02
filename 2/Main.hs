module Main where

import Control.Arrow
import Data.Semigroup
import Data.List
import System.IO

solveOne :: String -> Int
solveOne = sum . map (uncurry (-) . (getMax *** getMin) . foldMap (Max &&& Min) . map read . words) . lines

solveTwo :: String -> Int
solveTwo = sum . map (go . sort . map read . words) . lines
  where
    go :: [Int] -> Int
    go []     = 0
    go (x:xs) = case filter ((==0) . (flip mod) x) xs of
                  []   -> go xs
                  [x'] -> div x' x

main :: IO ()
main = do
  print =<< (solveOne &&& solveTwo) <$> readFile "input.txt"
