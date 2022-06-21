module Lib
    ( showWindow
    ) where

import qualified Control.Concurrent.STM as STM
import           Control.Monad
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

tickButtonWidth :: Int32
tickButtonWidth = 80

tickButtonHeight :: Int32
tickButtonHeight = 30

gridWidth :: Int
gridWidth = 300

gridHeight :: Int
gridHeight = 200

handleTick :: STM.TVar (STM.STM GameOfLife) -> IO ()
handleTick state = do
  STM.atomically $ do
    gol <- STM.readTVar state
    newGol <- gol >>= tick
    STM.writeTVar state $ return newGol

drawCanvas :: STM.TVar (STM.STM GameOfLife) -> Cairo.Render Bool
drawCanvas state = do
  clearCanvas
  drawGrid state

clearCanvas :: Cairo.Render Bool
clearCanvas = do
  Cairo.save
  Cairo.setSourceRGB 255 255 255
  Cairo.rectangle 0 0 300 200
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
              Cairo.rectangle (fromIntegral x) (fromIntegral y) 1 1
              Cairo.fill
              return True
          | otherwise = return True

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
  Gtk.widgetSetSizeRequest tickButton tickButtonWidth tickButtonHeight
  Gtk.fixedPut fixed tickButton 0 0

  redrawButton <- Gtk.buttonNewWithLabel (Text.pack "Redraw")
  Gtk.widgetSetSizeRequest tickButton tickButtonWidth tickButtonHeight
  Gtk.fixedPut fixed redrawButton tickButtonWidth 0

  drawingArea <- Gtk.drawingAreaNew
  Gtk.widgetSetHexpand drawingArea True
  Gtk.widgetSetVexpand drawingArea True
  Gtk.widgetSetSizeRequest drawingArea 300 200
  Gtk.fixedPut fixed drawingArea 0 tickButtonHeight

  gol <- STM.atomically $ STM.newTVar $ newGameOfLife gridWidth gridHeight

  Gtk.onButtonClicked tickButton (handleTick gol)
  Gtk.onButtonClicked redrawButton (Gtk.widgetQueueDrawArea drawingArea 0 0 300 200)
  Gtk.onWidgetDraw drawingArea (renderWithContext $ drawCanvas gol)

  Gtk.widgetShowAll win
  Gtk.main
