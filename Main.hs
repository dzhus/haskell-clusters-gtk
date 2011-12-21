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

type ClusteringMethod = [Cluster.Point] -> Double -> [Cluster]

clusterColors = cycle [red, green, blue, orange, steelblue,
                       teal, darkorchid, darkkhaki,
                       darkred, darkturquoise, brown, lightsteelblue,
                       hotpink, yellowgreen]

getLines = liftM lines . readFile

readPoint s = read s::Cluster.Point

coordSignificantDigits = 3

getPointsFromFile :: Maybe FilePath -> IO (Maybe [Cluster.Point])
getPointsFromFile Nothing = do return Nothing
getPointsFromFile (Just filename) =
  do lines <- getLines filename
     return (Just (map readPoint lines))

getPointsFromBuffer :: TextBuffer -> IO [Cluster.Point]
getPointsFromBuffer buf =
  do bounds <- textBufferGetBounds buf
     text <- textBufferGetText buf (fst bounds) (snd bounds) True
     if text /= "" then
          return (map readPoint (filter (/= "") $ lines text))
          else return []

-- given a group of "mode-mindist", "mode-kmeans" return a clustering
-- method to use
getModeByRadioGroup :: [RadioButton] -> IO (ClusteringMethod)
getModeByRadioGroup rbs =
  do status <- (toggleButtonGetActive $ head rbs)
     if status then
       return (\ps t -> Cluster.clusterizeMean ps t 100)
       else
       return (\ps t -> Cluster.clusterize ps t)
          
clearBuffer :: TextBuffer -> IO ()
clearBuffer buf =
  do textBufferSetText buf ""
     return ()

-- Reduce x to n significant digits after point
truncateSignificant :: Double -> Int -> Double
truncateSignificant x n =
  let
    t = fromIntegral (truncate (x * 10^n))
  in
    t / 10^n

-- Reduce point coordinates to n significant digits after point
truncatePoint :: Maybe Cluster.Point -> Int -> Maybe Cluster.Point
truncatePoint Nothing _ = Nothing
truncatePoint (Just p) n = Just (truncateSignificant (fst p) n,
                                 truncateSignificant (snd p) n)

-- Add point to list of points in a TextBuffer
addPoint :: Maybe Cluster.Point -> TextBuffer -> IO ()
addPoint Nothing buf = return ()
addPoint (Just p) buf =
  do end <- textBufferGetEndIter buf
     textBufferInsert buf end ("\n"++(show p))

-- Format result of PickFn call to Cluster.Point
pickToPoint :: (Maybe PickType) -> (Maybe Cluster.Point)
pickToPoint Nothing = Nothing
pickToPoint (Just (L1P_PlotArea x y z)) = Just (x, y)

-- Local version of updateCanvas which update PickFn reference
updateCanvas :: Renderable a -> DrawingArea -> IORef (Maybe (PickFn a)) -> IO Bool
updateCanvas chart canvas pickfv = do
     win <- widgetGetDrawWindow canvas
     (width, height) <- widgetGetSize canvas
     let sz = (fromIntegral width,fromIntegral height)
     pickf <- renderWithDrawable win $ runCRender (render chart sz) bitmapEnv
     writeIORef pickfv (Just pickf)
     return True

replotPoints pointBuffer adjustment canvas pickfv rbs= do
     points <- getPointsFromBuffer pointBuffer
     clusterSize <- adjustmentGetValue adjustment
     method <- getModeByRadioGroup rbs
     let clusters = (method points clusterSize) in
       Main.updateCanvas (renderClusters clusters) canvas pickfv
     return True

main :: IO ()
main = 
  do initGUI
     builder <- builderNew
     builderAddFromFile builder "window.ui"

     window <- builderGetObject builder castToWindow "window"
     canvas <- builderGetObject builder castToDrawingArea "canvas"
     button <- builderGetObject builder castToButton "go"
     clearButton <- builderGetObject builder castToButton "clear"
     textview <- builderGetObject builder castToTextView "textview"
     pointBuffer <- textViewGetBuffer textview
     size <- builderGetObject builder castToAdjustment "size"
     modeMindist <- builderGetObject builder castToRadioButton "mode-mindist"
     modeButtons <- radioButtonGetGroup modeMindist

     -- Pick function reference
     pickfv <- newIORef Nothing
     
     widgetShowAll window

     -- White canvas background
     drawin <- widgetGetDrawWindow canvas
     widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

     -- Event handlers
     let
       replotActions = (replotPoints pointBuffer size canvas pickfv modeButtons)
       in do
        onExpose canvas $ const replotActions
        onClicked button $ sequence_ [replotActions]

        onClicked clearButton $ (clearBuffer pointBuffer)
        
        onDestroy window mainQuit

        -- Add new point when clicking canvas
        onButtonPress canvas $ \(GE.Button{GE.eventX = x, GE.eventY = y}) -> do
           (Just pickf) <- readIORef pickfv
           let
             pick = (pickToPoint (pickf (Point x y)))
             in do
              addPoint (truncatePoint pick coordSignificantDigits) pointBuffer
              replotActions

     mainGUI

renderClusters :: [Cluster.Cluster] -> Renderable PickType
renderClusters clusters =
  layout1ToRenderable layout
  where
    -- Hidden plot to ensure visibility
    zero_plot = PlotHidden{plot_hidden_x_values_ = [-10, 10], plot_hidden_y_values_ = [-10, 10]}
    point_plots = map (\(c, color) ->
                         plot_points_style ^= filledCircles 5 (opaque color) $
                         plot_points_values ^= (Cluster.elements c) $
                         defaultPlotPoints)
                      (zip clusters clusterColors)
    clusters_plot = area_spots_fillcolour ^= (withOpacity blue 0.5) $
                    area_spots_max_radius ^= 5^2 $
                    area_spots_values ^= [(fst (Cluster.center c),
                                           snd (Cluster.center c),
                                           length (Cluster.elements c)) | c <- clusters] $ 
                    defaultAreaSpots
    layout = layout1_plots ^= (Left (toPlot zero_plot)):
                              (Left (toPlot clusters_plot)):
                              (map (Left . toPlot) point_plots)
                              $ defaultLayout1
