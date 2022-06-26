module Lib
    ( showWindow
    ) where

import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.Timer
import           Control.Concurrent.Suspend
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Array.IArray
import qualified Data.Text as Text
import           Data.GI.Base
import           GHC.Int
import qualified GI.Gtk as Gtk
import           GI.Cairo.Render.Connector (renderWithContext, toRender)
import qualified GI.Cairo.Render as Cairo

import           GameOfLife

contentWidth :: Int32
contentWidth = 800

contentHeight :: Int32
contentHeight = 600

buttonWidth :: Int32
buttonWidth = 80

buttonHeight :: Int32
buttonHeight = 30

canvasWidth :: Int32 
canvasWidth = 600

canvasHeight :: Int32 
canvasHeight = 300

gridWidth :: Int
gridWidth = 150

gridHeight :: Int
gridHeight = 100

gridMultiplier :: Int 
gridMultiplier = 4

tickTime :: Delay
tickTime = msDelay 500

data GameOfLifeState = GameOfLifeState {
  redrawFn :: IO (),
  gameOfLife :: STM.TVar (STM.STM GameOfLife),
  tickFn :: IO (),
  timer :: STM.TVar (IO (Maybe TimerIO))
}

handleTick :: GameOfLifeState -> IO ()
handleTick state = do
  let gol = gameOfLife state
  STM.atomically $ do
    stGol <- STM.readTVar gol
    newGol <- stGol >>= tick
    STM.writeTVar gol $ return newGol

handlePlay :: GameOfLifeState -> IO ()
handlePlay state = do
  let timerVar = timer state
  timer <- STM.readTVarIO timerVar
  timerVal <- timer
  case timerVal of 
    Nothing -> do newTimer <- repeatedTimer (tickFn state) tickTime
                  let wrappedTimer = return (Just newTimer)
                  STM.atomically $ STM.writeTVar timerVar wrappedTimer
    Just t ->  do stopTimer t
                  let newTimer = return Nothing
                  STM.atomically $ STM.writeTVar timerVar newTimer

drawCanvas gols = do
  clearCanvas
  drawGrid $ gameOfLife gols
  Cairo.liftIO $ redrawFn gols
  return True

clearCanvas :: Cairo.Render Bool
clearCanvas = do
  Cairo.save
  Cairo.setSourceRGB 255 255 255
  Cairo.rectangle 0 0 (fromIntegral canvasWidth) (fromIntegral canvasHeight)
  Cairo.fill
  Cairo.stroke
  Cairo.restore
  return True

drawGrid :: STM.TVar (STM.STM GameOfLife) -> Cairo.Render Bool
drawGrid state = do
  Cairo.save
  Cairo.setSourceRGB 0 0 0
  pixels <- Cairo.liftIO $ STM.atomically $ do
    stGol <- STM.readTVar state
    gol <- stGol
    g <- STM.readTVar $ grid gol
    return $ zip (indices g) (elems g)
  result <- foldM draw True pixels
  Cairo.restore
  return result
  where draw :: Bool -> ((Int, Int), CellState) -> Cairo.Render Bool
        draw s ((x, y), cellState)
          | cellState == Alive = do
              Cairo.rectangle x' y' (fromIntegral gridMultiplier) (fromIntegral gridMultiplier)
              Cairo.fill
              return True
          | otherwise = return True
          where x' = fromIntegral $ gridMultiplier * x
                y' = fromIntegral $ gridMultiplier * y

showWindow :: IO ()
showWindow = do
  Gtk.init Nothing

  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle win (Text.pack "Introduction")
  Gtk.windowResize win contentWidth contentHeight
  Gtk.onWidgetDestroy win Gtk.mainQuit

  fixed <- Gtk.fixedNew
  Gtk.containerAdd win fixed

  tickButton <- Gtk.buttonNewWithLabel (Text.pack "Tick")
  Gtk.widgetSetSizeRequest tickButton buttonWidth buttonHeight
  Gtk.fixedPut fixed tickButton 0 0

  playButton <- Gtk.buttonNewWithLabel (Text.pack "Play")
  Gtk.widgetSetSizeRequest playButton buttonWidth buttonHeight
  Gtk.fixedPut fixed playButton buttonWidth 0

  drawingArea <- Gtk.drawingAreaNew
  Gtk.widgetSetHexpand drawingArea True
  Gtk.widgetSetVexpand drawingArea True
  Gtk.widgetSetSizeRequest drawingArea canvasWidth canvasHeight
  Gtk.fixedPut fixed drawingArea 0 buttonHeight

  let timerVar = return Nothing
  gol <- STM.atomically $ STM.newTVar $ newGameOfLife gridWidth gridHeight
  timer <- STM.atomically $ STM.newTVar timerVar
  let gameOfLifeState = GameOfLifeState { 
    redrawFn = (Gtk.widgetQueueDrawArea drawingArea 0 0 canvasWidth canvasHeight), 
    gameOfLife = gol,
    tickFn = handleTick gameOfLifeState,
    timer = timer
  }

  Gtk.onButtonClicked tickButton (tickFn gameOfLifeState)
  Gtk.onButtonClicked playButton (handlePlay gameOfLifeState)
  Gtk.onWidgetDraw drawingArea (renderWithContext $ drawCanvas gameOfLifeState)

  Gtk.widgetShowAll win
  Gtk.main
