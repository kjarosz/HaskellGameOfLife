module GameOfLife
  ( GameOfLife
  ) where

import Control.Concurrent.STM
import Data.Array.IArray

data CellState = Alive | Dead deriving (Eq, Show, Enum)

type Grid = Array (Int, Int) CellState

data GameOfLife = GameOfLife {
  width :: Int,
  height :: Int,
  grid :: TVar Grid
}

newGameOfLife :: Int -> Int -> STM GameOfLife
newGameOfLife w h = do
  g <- newTVar $ createNewGrid w h
  return GameOfLife
    { width = w
    , height = h
    , grid = g }

createNewGrid :: Int -> Int -> Grid
createNewGrid w h = array ((0,0), (w-1, h-1)) [((i, j), Dead) | i <- [0..w-1], j <- [0..h-1]]
