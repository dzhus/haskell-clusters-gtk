module Main where

import Control.Monad
import Data.Accessor
import Data.IORef
import Data.Maybe
import IO

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as GE
import Graphics.Rendering.Cairo

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk

import Data.Colour
import Data.Colour.Names

import Cluster

type PickType = Layout1Pick Double Double

getLines = liftM lines . readFile

readPoint s = read s::Cluster.Point

getPointsFromFile :: Maybe FilePath -> IO (Maybe [Cluster.Point])
getPointsFromFile Nothing = do return Nothing
getPointsFromFile (Just filename) =
  do lines <- getLines filename
     return (Just (map readPoint lines))

getPointsFromBuffer :: TextBuffer -> IO (Maybe [Cluster.Point])
getPointsFromBuffer buf =
  do bounds <- textBufferGetBounds buf
     text <- textBufferGetText buf (fst bounds) (snd bounds) True
     if text /= "" then
          return (Just (map readPoint (filter (/= "") (lines text))))
          else return Nothing

-- Local version of updateCanvas which update PickFn reference
updateCanvas :: Renderable a -> DrawingArea -> IORef (Maybe (PickFn a)) -> IO Bool
updateCanvas chart canvas pickfv = do
     win <- widgetGetDrawWindow canvas
     (width, height) <- widgetGetSize canvas
     let sz = (fromIntegral width,fromIntegral height)
     pickf <- renderWithDrawable win $ runCRender (render chart sz) bitmapEnv
     writeIORef pickfv (Just pickf)
     return True

main :: IO ()
main = 
  do initGUI
     builder <- builderNew
     builderAddFromFile builder "window.ui"

     window <- builderGetObject builder castToWindow "window"
     canvas <- builderGetObject builder castToDrawingArea "canvas"
     button <- builderGetObject builder castToButton "go"
     textview <- builderGetObject builder castToTextView "textview"
     size <- builderGetObject builder castToAdjustment "size"

     -- Pick function reference
     pickfv <- newIORef Nothing
     
     widgetShowAll window

     -- White canvas background
     drawin <- widgetGetDrawWindow canvas
     widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

     onDestroy window mainQuit
     onClicked button (do buf <- textViewGetBuffer textview
                          points <- getPointsFromBuffer buf
                          clusterSize <- adjustmentGetValue size
                          let clusters = (clusterize (fromJust points) clusterSize) in
                              Main.updateCanvas (renderClusters clusters) canvas pickfv
                          return ())
     onButtonPress canvas $ \(GE.Button{GE.eventX=x,GE.eventY=y}) -> do
         print (x,y)
         (Just pickf) <- readIORef pickfv
         print (pickf (Point x y))
         return True
     mainGUI

renderClusters :: [Cluster.Cluster] -> Renderable PickType
renderClusters clusters =
  layout1ToRenderable layout
  where
    point_plots = [plot_points_style ^= filledCircles 5 (opaque red) $
                   plot_points_values ^= ((center c):(elements c)) $
                   defaultPlotPoints | c <- clusters]
    clusters_plot = area_spots_fillcolour ^= (withOpacity blue 0.5) $
                    area_spots_max_radius ^= 5^2 $
                    area_spots_values ^= [(fst (center c), snd (center c), threshold c) | c <- clusters] $ 
                    defaultAreaSpots
    layout = layout1_plots ^= (Left (toPlot clusters_plot)):(map (Left . toPlot) point_plots) $ defaultLayout1
