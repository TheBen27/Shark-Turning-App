{-# LANGUAGE OverloadedStrings #-}
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
  Gtk.init Nothing

  builder <- Gtk.builderNewFromFile "app-interface-alpha.glade"
  win <- Gtk.builderGetObject builder "mainWindow"

  --win <- Gtk.new Gtk.Window [ Gtk.windowTitle Gtk.:= "Glade demo" ]
  --Gtk.onWidgetDestroy win Gtk.mainQuit  

  --Gtk.widgetShowAll win
  Gtk.main
