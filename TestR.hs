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
  ds <- getDirectoryContents "/Users/dom/Downloads/KingstonuponThames"
  let es = filter (\x -> takeExtension x == ".shp") ds
  putStrLn $ show es
  R.runRegion $ do
    [r| library(rgdal) |]
    [r| library(ggplot2) |]
    let foo :: String
        foo = "/Users/dom/Downloads/KingstonuponThames" </> (es!!0)
    shapefile <- [r| readOGR(foo_hs) |]
    shapefile_df <- [r| fortify(shapefile_hs) |]
    let foo1 :: String
        foo1 = "/Users/dom/Downloads/KingstonuponThames" </> (es!!1)
    shapefile1 <- [r| readOGR(foo1_hs) |]
    shapefile1_df <- [r| fortify(shapefile1_hs) |]
    map1 <- [r| ggplot() +
                geom_polygon(data = shapefile_df_hs, 
                          aes(x = long, y = lat, group = group),
                          color = 'gray', fill = 'blue', size = .2) +
                geom_polygon(data = shapefile1_df_hs, 
                          aes(x = long, y = lat, group = group),
                          color = 'gray', fill = 'blue', size = .2) |]
    let foo2 :: String
        foo2 = "/Users/dom/Downloads/KingstonuponThames" </> (es!!2)
    shapefile2 <- [r| readOGR(foo2_hs) |]
    shapefile2_df <- [r| fortify(shapefile2_hs) |]
    map2 <- [r| map1_hs + 
                geom_polygon(data = shapefile2_df_hs, 
                         aes(x = long, y = lat, group = group),
                         color = 'gray', fill = 'blue', size = .2) |]
    -- [r| print(map2_hs) |]
    map_projected <- [r| map2_hs + coord_map() |]
    [r| print(map_projected_hs) |]
    -- [r| plot(np_dist_hs, col='red') |]
    -- zipWithM_
    --    (\f c -> do let foo :: String
    --                    foo = "/Users/dom/Downloads/KingstonuponThames" </> f
    --                np_dist <- [r| readOGR(foo_hs) |]
    --                [r| plot(np_dist_hs, add=TRUE, col=c_hs) |]
    --                return ()) (tail es) (cycle ["blue" :: String, "green", "yellow", "red"])
    return ()
  putStrLn "Finished"

