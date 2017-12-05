module Main where

import System.IO
import Data.Vector (Vector)
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V

run :: Int        -- ^ Number of jumps
    -> Int        -- ^ Current position
    -> Vector Int -- ^ Instructions
    -> Int        -- ^ Number of jumps
run n i v = case v  V.!? i of
              Nothing -> n
              Just i' ->
                run
                  (n + 1)
                  (i + i')
                  (V.modify (\v -> M.modify v go i) v)
  where
    -- For solution one: go = (+1)
    go :: Int -> Int
    go a
      | a >= 3    = a - 1
      | otherwise = a + 1

main :: IO ()
main =
  print =<< run 0 0 . V.fromList . map read . lines <$> readFile "input.txt"
