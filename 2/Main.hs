module Main where

import Control.Arrow
import Data.Semigroup hiding (diff)
import Data.List
import System.IO

diff :: Max Int -> Min Int -> Int
diff a b = (getMax a) - (getMin b)

solveOne :: String -> Int
solveOne = sum . map (uncurry diff . foldMap (Max &&& Min) . map read . words) . lines

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
