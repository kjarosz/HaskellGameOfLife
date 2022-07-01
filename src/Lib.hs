module Lib
    ( showWindow
    , gridWidth
    , gridHeight
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
import           Data.GI.Base.Overloading(IsDescendantOf)
import qualified GI.Gtk as Gtk
import           GI.Gtk.Enums
import qualified GI.Gdk as Gdk
import           GI.Cairo.Render.Connector (renderWithContext, toRender)
import qualified GI.Cairo.Render as Cairo

import           Constants
import           GameOfLife

data GameOfLifeState = GameOfLifeState {
  window :: Gtk.Window,
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

handleClearGrid :: GameOfLifeState -> IO ()
handleClearGrid state = do
  let gol = gameOfLife state
  STM.atomically $ do
    stGol <- STM.readTVar gol
    let newGol = stGol >>= clearGrid 
    STM.writeTVar gol newGol
  redrawFn state

handleDefaultGrid :: GameOfLifeState -> IO ()
handleDefaultGrid state = do
  let gol = gameOfLife state
  STM.atomically $ do
    let newGol = newGameOfLife gridWidth gridHeight
    STM.writeTVar gol newGol
  redrawFn state

handleLoadGrid :: GameOfLifeState -> IO ()
handleLoadGrid state = do
    let w = window state
    openFileDialog w loadGrid'
    redrawFn state
  where loadGrid' :: Gtk.FileChooserDialog -> FilePath -> IO Bool
        loadGrid' dialog filename = do
          result <- loadGrid filename
          case result of
            Left errorMsg -> do putStrLn errorMsg
                                return False
            Right grid -> do 
              STM.atomically $ do
                let golVar = gameOfLife state
                golSt <- STM.readTVar golVar
                gol <- golSt
                let newGol = setGrid gol grid
                STM.writeTVar golVar newGol
              return True

handleButtonPress :: GameOfLifeState -> Gdk.EventButton -> IO Bool
handleButtonPress state event = do
  let golVar = gameOfLife state
  button <- Gdk.getEventButtonButton event
  buttonX <- Gdk.getEventButtonX event
  buttonY <- Gdk.getEventButtonY event
  case button of 
    1 -> setCell' golVar (floor buttonX) (floor buttonY) Alive
    3 -> setCell' golVar (floor buttonX) (floor buttonY) Dead
  redrawFn state
  return True
  where toGridCoords c = c `div` gridMultiplier
        setCell' golVar x y state = do 
          STM.atomically $ do
            stGol <- STM.readTVar golVar
            gol <- stGol
            let newGol = setCell gol (toGridCoords x, toGridCoords y) state
            STM.writeTVar golVar newGol

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

  buttonBox <- Gtk.buttonBoxNew OrientationHorizontal
  Gtk.fixedPut fixed buttonBox 0 0

  tickButton <- makeButton buttonBox "Tick"
  playButton <- makeButton buttonBox "Play"
  clearButton <- makeButton buttonBox "Clear"
  defaultButton <- makeButton buttonBox "Default"
  loadButton <- makeButton buttonBox "Load"

  drawingArea <- Gtk.drawingAreaNew
  Gtk.widgetAddEvents drawingArea [Gdk.EventMaskButtonPressMask, Gdk.EventMaskPointerMotionMask]
  Gtk.widgetSetHexpand drawingArea True
  Gtk.widgetSetVexpand drawingArea True
  Gtk.widgetSetSizeRequest drawingArea canvasWidth canvasHeight
  Gtk.fixedPut fixed drawingArea 0 buttonHeight

  let timerVar = return Nothing
  gol <- STM.atomically $ STM.newTVar $ newGameOfLife gridWidth gridHeight
  timer <- STM.atomically $ STM.newTVar timerVar
  let gameOfLifeState = GameOfLifeState { 
    window = win,
    redrawFn = (Gtk.widgetQueueDrawArea drawingArea 0 0 canvasWidth canvasHeight), 
    gameOfLife = gol,
    tickFn = handleTick gameOfLifeState,
    timer = timer
  }

  Gtk.onButtonClicked tickButton (tickFn gameOfLifeState)
  Gtk.onButtonClicked playButton (handlePlay gameOfLifeState)
  Gtk.onButtonClicked clearButton (handleClearGrid gameOfLifeState)
  Gtk.onButtonClicked defaultButton (handleDefaultGrid gameOfLifeState)
  Gtk.onButtonClicked loadButton (handleLoadGrid gameOfLifeState)
  Gtk.onWidgetButtonPressEvent drawingArea (handleButtonPress gameOfLifeState)
  Gtk.onWidgetDraw drawingArea (renderWithContext $ drawCanvas gameOfLifeState)

  Gtk.widgetShowAll win
  Gtk.main

makeButton :: (Gtk.IsContainer a) => a -> String -> IO Gtk.Button
makeButton container title = do
  button <- Gtk.buttonNewWithLabel $ Text.pack title
  Gtk.widgetSetSizeRequest button buttonWidth buttonHeight
  Gtk.containerAdd container button
  return button

openFileDialog :: Gtk.Window -> ( Gtk.FileChooserDialog -> FilePath -> IO Bool) -> IO ()
openFileDialog window onSuccess = do
    dialog <- new Gtk.FileChooserDialog [] 
    Gtk.windowSetTitle dialog $ Text.pack "Open file"
    Gtk.windowSetTransientFor dialog $ Just window
    Gtk.fileChooserSetAction dialog Gtk.FileChooserActionOpen
    dialogAddButton dialog "gtk-cancel" Gtk.ResponseTypeCancel
    dialogAddButton dialog "Open" Gtk.ResponseTypeAccept
    Gtk.widgetShow dialog
    runDialog dialog
    Gtk.widgetDestroy dialog
  where runDialog dialog = do response <- dialogRun dialog
                              rerun <- handleResponse dialog response
                              if rerun then runDialog dialog
                                       else return ()
        handleResponse dialog Gtk.ResponseTypeAccept = do
          Just filename <- Gtk.fileChooserGetFilename dialog
          success <- onSuccess dialog filename
          return $ not success
        handleResponse _ _ = return False

dialogRun :: Gtk.IsDialog a => a -> IO Gtk.ResponseType 
dialogRun dialog = toEnum . fromIntegral <$> Gtk.dialogRun dialog 

dialogAddButton :: Gtk.IsDialog a => a -> String -> Gtk.ResponseType -> IO Gtk.Widget
dialogAddButton dialog text response = 
  Gtk.dialogAddButton dialog (Text.pack text) (fromIntegral $ fromEnum response)
