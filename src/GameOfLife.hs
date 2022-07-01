module GameOfLife
  ( CellState(..)
  , GameOfLife(..)
  , newGameOfLife
  , setGrid
  , clearGrid
  , setCell
  , tick
  , loadGrid
  ) where

import Control.Concurrent.STM
import Data.Array.IArray
import Data.List
import System.Directory
import System.IO

import ArrayUtil(createNewArray, createNewArrayFn, mapWithIndex, getElements, setElement)
import Constants(gridWidth, gridHeight)

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

setGrid :: GameOfLife -> Grid -> STM GameOfLife
setGrid gol newGrid = do
  let gridVar = grid gol
  writeTVar gridVar newGrid
  return gol

clearGrid :: GameOfLife -> STM GameOfLife
clearGrid gol = do
  let gridVar = grid gol
  writeTVar gridVar $ createNewArray (width gol) (height gol) Dead
  return gol

setCell :: GameOfLife -> (Int, Int) -> CellState -> STM GameOfLife
setCell gol index state = do
  let golVar = grid gol
  oldGrid <- readTVar golVar
  let newGrid = setElement oldGrid index state
  writeTVar golVar newGrid
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

loadGrid :: FilePath -> IO (Either String Grid)
loadGrid file = do
    fileExists <- doesFileExist file
    if fileExists then loadGrid' file
                  else return $ Left $ "File could not be found: " ++ file
  where loadGrid' :: FilePath -> IO (Either String Grid)
        loadGrid' file = do
          contents <- readFile file
          let gridList = transpose . mapRow . filterComments . lines $ contents
          let bounds = ((0, 0), (gridWidth-1, gridHeight-1))
          let elems = forceToGrid gridList
          if (length elems) == gridWidth * gridHeight 
             then return $ Right $ listArray bounds elems
             else return $ Left $ wrongElementLength elems

        wrongElementLength elems = (show (length elems)) ++ " actual, but " ++ (show (gridWidth * gridHeight)) ++ " expected."

        mapRow :: [String] -> [[CellState]]
        mapRow = map (map $ \x -> if (x == 'O') then Alive else Dead)

        filterComments :: [String] -> [String]
        filterComments ls = filter (not . isPrefixOf "!") ls

forceToGrid :: [[CellState]] -> [CellState]
forceToGrid ls = foldl (++) [] $ take gridHeight $ (map fillRow ls) ++ (repeat deadRow)
  where fillRow xs = take gridWidth $ xs ++ repeat Dead
        deadRow = take gridWidth $ repeat Dead
