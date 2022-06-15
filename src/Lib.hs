module Lib
    ( showWindow
    ) where

import           Data.Text
import           Data.GI.Base
import qualified GI.Gtk                        as Gtk

showWindow :: IO ()
showWindow = do
  Gtk.init Nothing

  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle win (pack "Introduction")
  Gtk.windowResize win 640 480
  Gtk.onWidgetDestroy win Gtk.mainQuit

  message <- Gtk.labelNew (Just (pack "Hello"))
  Gtk.containerAdd win message

  Gtk.widgetShowAll win
  Gtk.main
