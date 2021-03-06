{-# OPTIONS_GHC -Wall                    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeApplications     #-}

module Main (main) where

import Prelude as P

import qualified Language.R as R
import Language.R.QQ

import System.Directory
import System.FilePath

import Control.Monad
import Data.List (stripPrefix, nub)

import qualified Data.Foldable as Foldable
import Frames hiding ( (:&) )
import Control.Lens
import qualified Data.FuzzySet as Fuzzy
import Data.Text (pack)
import qualified Control.Foldl as Foldl
import Pipes.Prelude (fold)
import Text.Printf


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

tableTypes "ElectionResults" "data/Election Results 2018 v1.csv"

loadElectionResults :: IO (Frame ElectionResults)
loadElectionResults = inCoreAoS (readTable "data/Election Results 2018 v1.csv")

wardSet :: IO Fuzzy.FuzzySet
wardSet = loadElectionResults >>= f
  where
    f = return . Fuzzy.fromList . nub . Foldable.toList . fmap (^. ward)

wardFileIds :: IO [Text]
wardFileIds = do
  ds <- getDirectoryContents root
  let es = filter (\x -> takeExtension x == ".shp") ds
  let fs = case xs of
             Nothing -> error "Ward prefix does not match"
             Just ys -> ys
           where
             xs = sequence $
                  map (fmap dropExtension) $
                  map (stripPrefix "KingstonuponThames_") es
  return $ map pack fs

getWardData :: Text -> IO (Frame ElectionResults)
getWardData w = loadElectionResults >>= f
  where
    f = return . filterFrame (\x -> x ^. ward == w)

g :: Foldl.Fold ElectionResults [(Int, Int)]
g = sequenceA $
  map f
      [ "Berrylands"
      , "Norbiton"
      , "Old Malden"
      , "Coombe Hill"
      , "Beverley"
      , "Chessington North & Hook"
      , "Tolworth and Hook Rise"
      , "Coombe Vale"
      , "St James"
      , "Tudor"
      , "Alexandra"
      , "Canbury"
      , "Surbiton Hill"
      , "Chessington South"
      , "Grove"
      , "St Marks"
      ]
  where
    f w = (,) <$> (Foldl.handles (filtered (\x -> x ^. ward == w)) .
                   Foldl.handles votes) Foldl.sum
              <*> (Foldl.handles (filtered (\x -> x ^. ward == w &&
                                                  x ^. party == "Liberal Democrat")) .
                   Foldl.handles votes) Foldl.sum

main :: IO ()
main = do ns <- runSafeT $ Foldl.purely fold g (readTable "data/Election Results 2018 v1.csv")
          print ns
          mapM_ (printf "%.2f%%\n") $ map (\(x, y) -> (100.0 :: Double) * fromIntegral y / fromIntegral x) ns

-- main :: IO ()
-- main = do
--   ds <- getDirectoryContents root
--   let es = filter (\x -> takeExtension x == ".shp") ds
--   let fs = case xs of
--              Nothing -> error "Ward prefix does not match"
--              Just ys -> ys
--            where
--              xs = sequence $
--                   map (fmap dropExtension) $
--                   map (stripPrefix "KingstonuponThames_") es
--   x <- wardSet
--   y <- wardFileIds
--   error $ show $ sequence $ map (\f -> Fuzzy.getOne x f) y
--   let cs :: [String]
--       cs = case sequence $ map (\w -> lookup w percentLD) fs of
--              Nothing -> error "Wrong name in table"
--              Just xs -> map bucket xs
--   R.runRegion $ do
--     [r| library(rgdal) |]
--     [r| library(ggplot2) |]
--     [r| library(dplyr) |]
--     map0 <- [r| ggplot() |]
--     mapN <- foldM
--       (\m (f, c) -> do
--           let wward = root </> f
--           shapefileN <- [r| readOGR(wward_hs) |]
--           shapefileN_df <- [r| fortify(shapefileN_hs) |]
--           withColours <- [r| mutate(shapefileN_df_hs, colour=rep(c_hs, nrow(shapefileN_df_hs))) |]
--           [r| m_hs +
--               geom_polygon(data = withColours_hs,
--                            aes(x = long, y = lat, group = group, fill=colour),
--                            color = 'gray', size = .2) |]) map0 (zip es cs)
--     map_projected <- [r| mapN_hs + coord_map() +
--                          ggtitle("Kingston on Thames 2018 Local Election") +
--                          theme(legend.position="bottom") +
--                          scale_fill_manual(values=c("whitesmoke", "yellow1", "yellow2", "yellow3"),
--                                            name="Percentage Voting Lib Dem",
--                                            labels=c("0%-40% Lib Dem", "40%-50% Lib Dem", "50%-60% Lib Dem", "60%-100% Lib Dem")) |]
--     [r| png(filename="diagrams/kingston.png") |]
--     [r| print(map_projected_hs) |]
--     [r| dev.off() |]
--     return ()
--   putStrLn "Finished"
