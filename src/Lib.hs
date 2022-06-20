module Lib
    ( showWindow
    ) where

import           Data.Text
import           Data.GI.Base
import           GHC.Int
import qualified GI.Gtk                        as Gtk
import           GI.Cairo.Render.Connector (renderWithContext, toRender)
import qualified GI.Cairo.Render as Cairo

import           GameOfLife

contentWidth :: Int32
contentWidth = 800

contentHeight :: Int32
contentHeight = 600

fillBackground :: Cairo.Render Bool
fillBackground = do
  Cairo.save
  Cairo.setSourceRGB 0 0 0
  Cairo.rectangle 0 0 300 200
  Cairo.fill
  Cairo.stroke
  Cairo.restore
  return True

showWindow :: IO ()
showWindow = do
  Gtk.init Nothing

  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle win (pack "Introduction")
  Gtk.windowResize win contentWidth contentHeight
  Gtk.onWidgetDestroy win Gtk.mainQuit

  drawingArea <- Gtk.drawingAreaNew
  Gtk.widgetSetHexpand drawingArea True
  Gtk.widgetSetVexpand drawingArea True
  Gtk.containerAdd win drawingArea

  Gtk.onWidgetDraw drawingArea (renderWithContext fillBackground)

  Gtk.widgetShowAll win
  Gtk.main
