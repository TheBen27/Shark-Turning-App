{-
 - Plot something with charts
 -}
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

linePlot :: EC (Layout Int Int) ()
linePlot = plot (line "X's" [ [(x,x*x) :: (Int,Int) | x <- [0..10]] ] )

main = toFile def "chart-out.png" $ do
  layout_title .= "Chart Title"
  linePlot

