{-
 - Renders something from Cairo into a GTK application
 -}
{-# LANGUAGE OverloadedStrings #-}
import qualified GI.Cairo as GI_Cairo
import qualified GI.Gtk as Gtk
import GI.Gtk hiding (main)
import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Internal as Cairo_Int

import Control.Monad.Reader
import Foreign.Ptr 

main :: IO ()
main = do
  Gtk.init Nothing
  win <- new Window [ ]
  onWidgetDestroy win mainQuit
  canvas <- drawingAreaNew
  
  containerAdd win canvas

  onWidgetDraw canvas (\con -> renderWithContext renderImage con >> return True)
  
  widgetShowAll win
  Gtk.main

renderImage :: Render ()
renderImage = setSourceRGB 1 0 0 >> paint

{-
 - This code converts from Haskell's cairo library to GI.Cairo introspection.
 -}
renderWithContext :: Render () -> GI_Cairo.Context -> IO ()
renderWithContext r ct = withManagedPtr ct $ \p -> runReaderT (Cairo_Int.runRender r) (Cairo_Int.Cairo (castPtr p))
