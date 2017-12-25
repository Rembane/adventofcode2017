{-# LANGUAGE BangPatterns #-}

module Main where

import           Data.List
import           Debug.Trace
import qualified Data.Vector.Unboxed as V

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

solveOne' :: IO ()
solveOne' =
  case V.elemIndex 2017 v of
    Nothing -> putStrLn "Couldn't find 2017."
    Just i  -> print (v V.! (i + 1))
  where
    v = insertManyValues puzzleInput 0 [1..2017] (V.singleton 0)

    nextIndex :: Int -> Int -> Int -> Int
    nextIndex input len idx = (mod (idx + input) len) + 1

    insertValue :: Int -> Int -> Int -> V.Vector Int -> (Int, V.Vector Int)
    insertValue input currentIdx value vector =
      let idx'     = nextIndex input (V.length vector) currentIdx
          (v1, v2) = V.splitAt idx' vector
       in (idx', V.force $ V.concat [v1, (V.singleton value), v2])

    insertManyValues :: Int -> Int -> [Int] -> V.Vector Int -> V.Vector Int
    insertManyValues _     _          []     vector = vector
    insertManyValues input currentIdx (x:xs) vector =
      let (idx', vector') = insertValue input currentIdx x vector
       in insertManyValues input idx' xs vector'

solveTwo' :: IO ()
solveTwo' = print $ go 0 1
  where
    go :: Int -> Int -> [Int]
    go _   50000001 = []
    go idx len      =
      case idx' of
        1 -> len : step
        _ ->       step
      where
        idx' = (mod (idx + puzzleInput) len) + 1
        step = go idx' (len + 1)

main :: IO ()
main = solveOne >> solveTwo'
