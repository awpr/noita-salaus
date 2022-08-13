{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-} -- shush a warning on simplifiable constraints
{-# LANGUAGE TypeOperators #-}

module Salaus.Plot where

import Data.Semigroup (getSum)
import System.Process (callCommand)

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy

import Data.IntMap.Keyed qualified as KM
import Data.Type.Attenuation (type (⊆), attenuate)

import Salaus.Distribution

viewPlot :: (Default r, ToRenderable r) => EC r () -> IO ()
viewPlot ec = do
  toFile (def & fo_size .~ (1920, 1080)) "/tmp/plot.svg" ec
  callCommand "feh /tmp/plot.svg"

plotHistograms :: a ⊆ Int => [String] -> [Histogram a] -> EC (Layout Int Int) ()
plotHistograms nms h = do
  layout_title .= "histogram"
  p <- bars nms
    [ (attenuate k :: Int, getSum <$> vs)
    | (k, vs) <- KM.toList $ KM.unionsWith (++) $ map (fmap pure) h
    ]
  plot $ return $ plotBars (p & plot_bars_spacing .~ BarsFixWidth 6)

plotIoCSweep :: String -> [String] -> [(Int, [Float])] -> EC (Layout Int Double) ()
plotIoCSweep title series iocs = do
  layout_title .= title
  p <- bars series [(x, map realToFrac ks) | (x, ks) <- iocs]
  plot $ return $ plotBars (p & plot_bars_spacing .~ BarsFixWidth 6)
