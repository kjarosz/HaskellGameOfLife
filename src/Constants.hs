module Constants where

import           Control.Concurrent.Suspend
import           GHC.Int
import qualified System.FilePath as Path

contentWidth :: Int32
contentWidth = 800

contentHeight :: Int32
contentHeight = 600

buttonWidth :: Int32
buttonWidth = 80

buttonHeight :: Int32
buttonHeight = 30

canvasWidth :: Int32 
canvasWidth = fromIntegral $ gridWidth * gridMultiplier

canvasHeight :: Int32 
canvasHeight = fromIntegral $ gridHeight * gridMultiplier

gridWidth :: Int
gridWidth = 150

gridHeight :: Int
gridHeight = 100 

gridMultiplier :: Int 
gridMultiplier = 6

tickTime :: Delay
tickTime = msDelay 60
