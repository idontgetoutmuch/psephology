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
import Data.List (stripPrefix)


root :: FilePath
root = "data/KingstonuponThames"

percentLD :: [(String, Int)]
percentLD = [ ("Alexandra", 56)
            , ("Berrylands", 57)
            , ("Beverley", 50)
            , ("Canbury", 49)
            , ("ChessingtonNorthandHook", 55)
            , ("ChessingtonSouth", 55)
            , ("CoombeHill", 26)
            , ("CoombeVale", 43)
            , ("Grove", 58)
            , ("Norbiton", 56)
            , ("OldMalden", 35)
            , ("St_James", 52)
            , ("St_Mark's", 60)
            , ("SurbitonHill", 64)
            , ("TolworthandHookRise", 66)
            , ("Tudor", 39)
            ]

bucket :: Int -> String
bucket x | x < 40    = "whitesmoke"
         | x < 50    = "yellow1"
         | x < 60    = "yellow2"
         | otherwise = "yellow3"

main :: IO ()
main = do
  ds <- getDirectoryContents root
  let es = filter (\x -> takeExtension x == ".shp") ds
  let fs = case xs of
             Nothing -> error "Ward prefix does not match"
             Just ys -> ys
           where
             xs = sequence $
                  map (fmap dropExtension) $
                  map (stripPrefix "KingstonuponThames_") es
  let cs :: [String]
      cs = case sequence $ map (\w -> lookup w percentLD) fs of
             Nothing -> error "Wrong name in table"
             Just xs -> map bucket xs
  R.runRegion $ do
    [r| library(rgdal) |]
    [r| library(ggplot2) |]
    [r| library(dplyr) |]
    map0 <- [r| ggplot() |]
    mapN <- foldM
      (\m (f, c) -> do
          let ward = root </> f
          shapefileN <- [r| readOGR(ward_hs) |]
          shapefileN_df <- [r| fortify(shapefileN_hs) |]
          withColours <- [r| mutate(shapefileN_df_hs, colour=rep(c_hs, nrow(shapefileN_df_hs))) |]
          [r| m_hs +
              geom_polygon(data = withColours_hs,
                           aes(x = long, y = lat, group = group, fill=colour),
                           color = 'gray', size = .2) |]) map0 (zip es cs)
    map_projected <- [r| mapN_hs + coord_map() +
                         ggtitle("Kingston on Thames 2018 Local Election") +
                         theme(legend.position="bottom") +
                         scale_fill_manual(values=c("whitesmoke", "yellow1", "yellow2", "yellow3"),
                                           name="Percentage Voting Lib Dem",
                                           labels=c("0%-40% Lib Dem", "40%-50% Lib Dem", "50%-60% Lib Dem", "60%-100% Lib Dem")) |]
    [r| png(filename="diagrams/kingston.png") |]
    [r| print(map_projected_hs) |]
    [r| dev.off() |]
    return ()
  putStrLn "Finished"
