{-# OPTIONS_GHC -Wall                    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Main (main) where

import Prelude as P

import qualified Language.R as R
import Language.R.QQ

import System.Directory
import System.FilePath

import Control.Monad

root :: FilePath
root = "data/KingstonuponThames"

main :: IO ()
main = do
  ds <- getDirectoryContents root
  let es = filter (\x -> takeExtension x == ".shp") ds
  R.runRegion $ do
    [r| library(rgdal) |]
    [r| library(ggplot2) |]
    map0 <- [r| ggplot() |]
    mapN <- foldM
      (\m f -> do let ward = root </> f
                  shapefileN <- [r| readOGR(ward_hs) |]
                  shapefileN_df <- [r| fortify(shapefileN_hs) |]
                  [r| m_hs + 
                      geom_polygon(data = shapefileN_df_hs, 
                                   aes(x = long, y = lat, group = group),
                                   color = 'gray', fill = 'blue', size = .2) |]) map0 es
    map_projected <- [r| mapN_hs + coord_map() |]
    [r| png(filename="diagrams/kingston.png") |]
    [r| print(map_projected_hs) |]
    [r| dev.off() |]
    return ()
  putStrLn "Finished"

