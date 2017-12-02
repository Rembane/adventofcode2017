module Main where

import Control.Arrow
import Data.Monoid
import Data.List
import System.IO

newtype Maximum = Maximum Int
  deriving (Bounded, Eq, Ord, Show)
newtype Minimum = Minimum Int
  deriving (Bounded, Eq, Ord, Show)

instance Monoid Maximum where
  mappend = max
  mempty  = minBound

instance Monoid Minimum where
  mappend = min
  mempty  = maxBound

diff :: Maximum -> Minimum -> Int
diff (Maximum a) (Minimum b) = a - b

solveOne :: String -> Int
solveOne = sum . map (uncurry diff . foldMap (Maximum &&& Minimum) . map read . words) . lines

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
