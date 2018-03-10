{-# LANGUAGE OverloadedStrings #-}
import qualified GI.Gtk as Gtk
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  Gtk.init Nothing

  builder <- Gtk.builderNewFromFile "app-interface-alpha.glade"
  win <- Gtk.builderGetObject builder "mainWindow" >>= unsafeCastTo Gtk.Window . fromJust
  Gtk.onWidgetDestroy win Gtk.mainQuit  
  
  Gtk.widgetShowAll win
  Gtk.main

  --win <- Gtk.new Gtk.Window [ Gtk.windowTitle Gtk.:= "Glade demo" ]
