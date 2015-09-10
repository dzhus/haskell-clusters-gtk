module Main where

import Control.Monad
import Control.Lens
import Data.IORef
import Data.List

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as GE

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Colour
import Data.Colour.Names

import Data.Default.Class

import Cluster
import Perceptron

type PickType = LayoutPick Double Double Double

type ClusteringMethod = [Cluster.Point] -> Double -> [Cluster]

clusterColors :: [Colour Double]
clusterColors = cycle [red, green, blue, orange, steelblue,
                       teal, darkorchid, darkkhaki,
                       darkred, darkturquoise, brown, lightsteelblue,
                       hotpink, yellowgreen]

getLines :: FilePath -> IO [String]
getLines = liftM lines . readFile

readPoint :: String -> Cluster.Point
readPoint = read

coordSignificantDigits :: Int
coordSignificantDigits = 3

getPointsFromFile :: Maybe FilePath -> IO (Maybe [Cluster.Point])
getPointsFromFile Nothing = do return Nothing
getPointsFromFile (Just filename) =
  do lns <- getLines filename
     return $ Just $ map readPoint lns

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
pickToPoint (Just (LayoutPick_PlotArea x y z)) = Just (x, y)

-- Local version of updateCanvas which updates PickFn reference
updateCanvas :: Renderable a -> DrawingArea -> IORef (Maybe (PickFn a)) -> IO Bool
updateCanvas chart canvas pickfv = do
     win <- widgetGetDrawWindow canvas
     (width, height) <- widgetGetSize canvas
     let sz = (fromIntegral width,fromIntegral height)
         env = defaultEnv bitmapAlignmentFns
     pickf <- renderWithDrawable win $ runBackend env (render chart sz)
     writeIORef pickfv (Just pickf)
     return True

-- Redraw current clustering
replotPoints pointBuffer adjustment canvas fillBounds pickfv rbs = do
     points <- getPointsFromBuffer pointBuffer
     clusterSize <- adjustmentGetValue adjustment
     method <- getModeByRadioGroup rbs
     doBounds <- toggleButtonGetActive fillBounds
     let
         clusters = (method points clusterSize)
         ys = map snd points
         yRange = (minimum ys, maximum ys)
       in
         Main.updateCanvas (renderClusters clusters doBounds yRange) canvas pickfv
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
     fillBounds <- builderGetObject builder castToCheckButton "bounds"

     -- Pick function reference
     pickfv <- newIORef Nothing

     widgetShowAll window

     -- White canvas background
     drawin <- widgetGetDrawWindow canvas
     widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

     -- Event handlers
     let
       replotActions = (replotPoints pointBuffer size canvas fillBounds pickfv modeButtons)
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

renderClusters :: [Cluster.Cluster] -> Bool -> (Double, Double) -> Renderable PickType
renderClusters clusters withBounds yRange =
    layoutToRenderable layout
  where
    hiddenRange@(hMin, hMax) = (-10, 10)

    bounds = if withBounds
             then map (\[c1, c2] ->
                 let
                     (w1, w2, w3) = Perceptron.bisect c1 c2 0.5
                     yMin = min (fst hiddenRange) (fst yRange)
                     yMax = max (snd hiddenRange) (snd yRange)
                     d1 = ((w2 * (-yMin) - w3) / w1, yMin)
                     d2 = ((w2 * (-yMax) - w3) / w1, yMax)
                 in
                   [d1, d2]) (filter ((2 ==) . length) (subsequences clusters))
             else []

    -- Hidden plot to ensure visibility
    -- TODO Use lens
    zero_plot = PlotHidden{_plot_hidden_x_values = [hMin, hMax], _plot_hidden_y_values = [hMin, hMax]}

    -- Classification lines
    line_plot = plot_lines_values .~ bounds $ (def :: PlotLines Double Double)

    point_plots = map (\(c, color) ->
                         plot_points_style .~ filledCircles 5 (opaque color) $
                         plot_points_values .~ (Cluster.elements c) $
                         def)
                      (zip clusters clusterColors)
    clusters_plot = area_spots_fillcolour .~ blue $
                    area_spots_opacity .~ 0.5 $
                    area_spots_max_radius .~ 5^2 $
                    area_spots_values .~ [(fst (Cluster.center c),
                                           snd (Cluster.center c),
                                           length (Cluster.elements c)) | c <- clusters] $
                    def
    layout = layout_plots .~ (toPlot zero_plot):
                             (toPlot line_plot):
                             (toPlot clusters_plot):
                             (map toPlot point_plots) $
                             def
