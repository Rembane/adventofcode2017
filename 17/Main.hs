{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List

puzzleInput :: Int
puzzleInput = 335

data Buffer a = Buffer !Int ![a] ![a]
  deriving Show

getLength :: Buffer a -> Int
getLength (Buffer l _ _) = l

binsert :: a -> Buffer a -> Buffer a
binsert x (Buffer len p n) = Buffer (len + 1) (x:p) n

stepRight :: Buffer a -> Buffer a
stepRight (Buffer l ps [])     = stepRight (Buffer l [] (reverse ps))
stepRight (Buffer l ps (x:xs)) =            Buffer l (x:ps) xs

stepMany :: Int -> Buffer a -> Buffer a
stepMany n b = foldl' (\b' _ -> stepRight b') b [1..mod n (getLength b)]

insertMany :: Int -> [Int] -> Buffer Int -> Buffer Int
insertMany n xs b = foldl' (\b' x -> binsert x (stepMany n b')) b xs

solveOne :: IO ()
solveOne = let (Buffer _ _ (x:xs)) = insertMany 335 [1..2017] (Buffer 1 [] [0])
            in print x

loadTest :: IO ()
loadTest = let (Buffer _ _ (x:xs)) = insertMany 335 [1..100000] (Buffer 1 [] [0])
            in print x

solveTwo :: IO ()
solveTwo = let (Buffer _ a b) = insertMany 335 [1..50000000] (Buffer 1 [] [0])
               f = drop 1 . dropWhile (/= 0)
            in print $ head $ (f b) ++ (f $ reverse a)

main :: IO ()
main = loadTest
-- solveOne -- >> solveTwo
