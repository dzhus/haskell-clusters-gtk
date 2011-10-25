module Main where

import Control.Monad
import Data.Accessor
import Data.Maybe
import IO

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk

import Data.Colour
import Data.Colour.Names

import Cluster

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
     
     widgetShowAll window

     drawin <- widgetGetDrawWindow canvas
     widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

     onDestroy window mainQuit
     onClicked button (do buf <- textViewGetBuffer textview
                          points <- getPointsFromBuffer buf
                          clusterSize <- adjustmentGetValue size
                          let clusters = (clusterize (fromJust points) clusterSize) in
                              updateCanvas (toRenderable (plotClusters clusters)) canvas
                          return ())
     mainGUI

plotClusters :: [Cluster.Cluster] -> Layout1 Double Double
plotClusters clusters =
  layout
  where
    point_plots = [plot_points_style ^= filledCircles 5 (opaque red) $
                   plot_points_values ^= ((center c):(elements c)) $
                   defaultPlotPoints | c <- clusters]
    clusters_plot = area_spots_fillcolour ^= (withOpacity blue 0.5) $
                    area_spots_max_radius ^= 5^2 $
                    area_spots_values ^= [(fst (center c), snd (center c), threshold c) | c <- clusters] $ 
                    defaultAreaSpots
    layout = layout1_plots ^= (Left (toPlot clusters_plot)):(map (Left . toPlot) point_plots) $ defaultLayout1
