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
  Gtk.onWidgetDestroy win Gtk.mainQuit
  Gtk.widgetShowAll win

  Gtk.main
