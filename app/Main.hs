{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Csv
import Data.List (sortBy)
import Data.Ord (Down(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Lib
import Renderable
import Util

instance FromNamedRecord VStats where
    -- TODO msrp
    parseNamedRecord r = VStats <$> r .: "car"
                                <*> r .: "weight"
                                <*> r .: "bhp"
                                <*> r .: "worth"

loadVehicles :: FilePath -> IO [VStats]
loadVehicles path = do
    csvData <- BL.readFile path
    case decodeByName csvData of
        Left err -> putStrLn err >> return []
        Right (_, v) -> return (V.toList v)

orderF = (*(-1)) . spd
orderV v1 v2 = compare (orderF v1) (orderF v2)
-- orderTV (TV v1 _ cpy) (TV v2 _ cpy') = compare (orderF v1) (orderF v2)
orderTV (TV v1 _ cpy) (TV v2 _ cpy') = compare cpy cpy'

-- tracked = sortBy orderTV [ trackV (modV c5z "track" 0 400 5000) 400 8 400,
--     trackV (modV frs "NA" (-100) 190 10000) 250 4 250,
--     trackV (modV frs "race" (-300) 200 15000) 250 4 250,
--     trackV (modV frs "JRSC" 0 240 10000) 350 4 250,
--     trackV (modV frs "JRSC race" 0 260 20000) 350 4 400,
--     trackV (modV frs "JRSC +e85" 0 300 20000) 500 8 250,
--     trackV (modV wrx "cobb" 0 300 5000) 350 6 350,
--     trackV (modV s2k "NA" (-100) 200 5000) 250 4 300,
--     trackV (modV miata "race" (-200) 160 10000) 200 4 200 ]

main = do
    putStrLn "Stock"
    render =<< loadVehicles "cars/stock.csv"

    -- putStrLn "Tracked"
    -- render =<< loadVehicles "cars/modded.csv"

    -- calculate some useful statistics
    -- putStrLn "Fastest street car / $"
    -- putStrLn $ show (findLastOrd (\v v' -> compare (spd v) (spd v')) vehs)
    -- putStrLn "Fastest track car / $"
    -- putStrLn $ show (findLastOrd (\v v' -> compare (spd $ tstats v) (spd $ tstats v')) tracked)
    -- putStrLn "Cheapest track car w/ consumables"
    -- TODO
