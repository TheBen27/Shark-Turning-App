{-
 - Take a chart from the Haskell libraries and push it into
 - a Gtk application
 -}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified GI.Cairo as GI_Cairo
import qualified GI.Gtk as Gtk 
import GI.Gtk hiding (main, Layout)
import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Internal as Cairo_Int

import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

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

linePlot :: EC (Layout Int Int) ()
linePlot = plot (line "X's" [ [(x,x*x) :: (Int,Int) | x <- [0..10]] ] )

{-
 - Plots have a type of EC r (), where r is toRenderable
 - execEC can get you the r, then toRenderable can get you a Renderable
 -
 - some function called render r (width, height) creates a backendProgram
 -
 - runBackend :: CEnv -> BackendProgram a -> C.Render a
 -
 - defaultEnv :: AlignmentFns -> CEnv
 -
 - bitmapAlignmentFns width height -- Graphics.Rendering.Chart.Backend
 -}
renderImage :: Cairo_Int.Render ()
renderImage =
  let dims@(width, height) = (250, 250)
      env = defaultEnv bitmapAlignmentFns
      ren = toRenderable linePlot
      prg = render ren dims
  in runBackend env prg >> return ()

{-
 - This code converts from Haskell's cairo library to GI.Cairo introspection.
 -}
renderWithContext :: Render () -> GI_Cairo.Context -> IO ()
renderWithContext r ct = withManagedPtr ct $ \p -> runReaderT (Cairo_Int.runRender r) (Cairo_Int.Cairo (castPtr p))
