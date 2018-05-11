{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-# LANGUAGE DataKinds,
 FlexibleContexts,
 TemplateHaskell,
 TypeOperators,
 TypeSynonymInstances,
 FlexibleInstances,
 QuasiQuotes,
 UndecidableInstances,
 ExplicitForAll,
 ScopedTypeVariables,
 OverloadedStrings,
 GADTs,
 TypeApplications
#-}

module Main (main) where

import Prelude as P

import qualified Language.R as R
import Language.R.QQ

import System.Directory
import System.FilePath

import Control.Monad

main :: IO ()
main = do
  ds <- getDirectoryContents "data//KingstonuponThames"
  let es = filter (\x -> takeExtension x == ".shp") ds
  putStrLn $ show es
  R.runRegion $ do
    [r| library(rgdal) |]
    [r| library(ggplot2) |]
    let foo :: String
        foo = "data//KingstonuponThames" </> (es!!0)
    shapefile <- [r| readOGR(foo_hs) |]
    shapefile_df <- [r| fortify(shapefile_hs) |]
    let foo1 :: String
        foo1 = "data//KingstonuponThames" </> (es!!1)
    shapefile1 <- [r| readOGR(foo1_hs) |]
    shapefile1_df <- [r| fortify(shapefile1_hs) |]
    map1 <- [r| ggplot() +
                geom_polygon(data = shapefile_df_hs, 
                          aes(x = long, y = lat, group = group),
                          color = 'gray', fill = 'blue', size = .2) +
                geom_polygon(data = shapefile1_df_hs, 
                          aes(x = long, y = lat, group = group),
                          color = 'gray', fill = 'blue', size = .2) |]
    mapN <- foldM
      (\m f -> do let bar :: String
                      bar = "data/KingstonuponThames" </> f
                  shapefileN <- [r| readOGR(bar_hs) |]
                  shapefileN_df <- [r| fortify(shapefileN_hs) |]
                  [r| m_hs + 
                      geom_polygon(data = shapefileN_df_hs, 
                                   aes(x = long, y = lat, group = group),
                                   color = 'gray', fill = 'blue', size = .2) |]) map1 (drop 2 es)
    map_projected <- [r| mapN_hs + coord_map() |]
    [r| print(map_projected_hs) |]
    return ()
  putStrLn "Finished"

