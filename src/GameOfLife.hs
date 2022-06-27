module GameOfLife
  ( CellState(..)
  , GameOfLife(..)
  , newGameOfLife
  , clearGrid
  , tick
  ) where

import Control.Concurrent.STM
import Data.Array.IArray

import ArrayUtil(createNewArray, createNewArrayFn, mapWithIndex, getElements)

data CellState = Alive | Dead deriving (Eq, Show, Enum)

type Grid = Array (Int, Int) CellState

data GameOfLife = GameOfLife {
  width :: Int,
  height :: Int,
  grid :: TVar Grid
}

newGameOfLife :: Int -> Int -> STM GameOfLife
newGameOfLife w h = do
  g <- newTVar $ createNewArrayFn w h initializer
  return GameOfLife
    { width = w
    , height = h
    , grid = g }
  where initializer (x, y)
          | mod (x + y) 3 == 0 = Alive
          | otherwise = Dead

clearGrid :: GameOfLife -> STM GameOfLife
clearGrid gol = do
  let gridVar = grid gol
  writeTVar gridVar $ createNewArray (width gol) (height gol) Dead
  return gol

tick :: GameOfLife -> STM GameOfLife
tick gol = do
  g <- readTVar $ grid gol
  writeTVar (grid gol) $ mapWithIndex deadOrAlive g
  return gol

deadOrAlive :: Grid -> (Int, Int) -> CellState -> CellState
deadOrAlive grid (x, y) state
  | state == Dead && liveNeighbors == 3 = Alive
  | state == Alive && liveNeighbors `elem` [2,3] = Alive
  | otherwise = Dead
  where neighbors = filter (\(a, b) -> (a, b) /= (x, y)) [(x+a, y+b) | a <- [-1..1], b <- [-1..1]]
        liveNeighbors = length . filter (\(e) -> e == Alive) $ getElements grid neighbors
