module Main where

import Data.List

puzzleInput :: Int
puzzleInput = 335

newtype Buffer a = Buffer ([a], [a])
  deriving Show

binsert :: a -> Buffer a -> Buffer a
binsert x (Buffer (p, n)) = Buffer (x:p, n)

stepRight :: Buffer a -> Buffer a
stepRight (Buffer (ps, []))     = stepRight (Buffer ([], reverse ps))
stepRight (Buffer (ps, (x:xs))) =            Buffer (x:ps, xs)

stepMany :: Int -> Buffer Int -> Buffer Int
stepMany 0 b = b
stepMany n b = stepMany (n-1) (stepRight b)

insertOne :: Int -> Int -> Buffer Int -> Buffer Int
insertOne n x b = binsert x (stepMany n b)

insertMany :: Int -> [Int] -> Buffer Int -> Buffer Int
insertMany n xs b = foldl' (flip $ insertOne n) b xs

solveOne :: IO ()
solveOne = let (Buffer (_, x:xs)) = insertMany 335 [1..2017] (Buffer ([],  [0]))
            in print x

main :: IO ()
main = solveOne
