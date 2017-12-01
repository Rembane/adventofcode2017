module Main where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Vector as V
import System.IO

solutionOne :: IO ()
solutionOne = do
  (i:is) <- map digitToInt . filter (/= '\n') <$> readFile "input.txt"
  print . sum . join . map tail . filter ((> 1) . length) . group $ i:is ++ [i]

solutionTwo :: IO ()
solutionTwo = do
  is <- V.fromList . map digitToInt . filter (/= '\n') <$> readFile "input.txt"
  let len  = V.length is
  let jump = len `div` 2

  print $ sum $ V.ifilter (\i a -> a == (is V.! (mod (i + jump) len))) is

main :: IO ()
main = solutionTwo
