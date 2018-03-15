{-
 - Simple cairo drawing example
 -}
import Graphics.Rendering.Cairo

makeImage :: Int -> Int -> String -> Render () -> IO ()
makeImage width height filename render =
  withImageSurface FormatRGB24 width height
    (\surface -> do renderWith surface render
                    surfaceWriteToPNG surface filename)

main :: IO ()
main = makeImage 256 256 "cairo-out.png" $ do
  setSourceRGB 1 0 0
  paint
