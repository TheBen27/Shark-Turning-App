{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

import qualified GI.Gtk as Gtk
import Data.GI.Base

main :: IO ()
main = do
  Gtk.init Nothing

  win <- new Gtk.Window [ Gtk.windowTitle := "Hi there" ]

  Gtk.onWidgetDestroy win Gtk.mainQuit

  button <- new Gtk.Button [ Gtk.buttonLabel := "Click me"  ]

  Gtk.onButtonClicked button (set button [ Gtk.widgetSensitive := False,
                                           Gtk.buttonLabel := "Thanks"])
  

  Gtk.containerAdd win button
  Gtk.widgetShowAll win
  Gtk.main
