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
    parseNamedRecord r = VStats <$> r .: "car"
                                <*> r .: "weight"
                                <*> r .: "bhp"
                                <*> r .: "msrp"
                                <*> r .: "worth"
                                <*> r .: "mods"

loadVehicles :: FilePath -> IO [VStats]
loadVehicles path = do
    csvData <- BL.readFile path
    case decodeByName csvData of
        Left err -> putStrLn err >> return []
        Right (_, v) -> return (V.toList v)

orderV v1 v2 = compare (f v1) (f v2)
    where f = (*(-1)) . spd

main = do
    putStrLn "Stock"
    render . sortBy orderV =<< loadVehicles "cars/stock.csv"

    putStrLn "Tracked"
    render . sortBy orderV =<< loadVehicles "cars/modded.csv"

    -- calculate some useful statistics
    -- putStrLn "Fastest street car / $"
    -- putStrLn $ show (findLastOrd (\v v' -> compare (spd v) (spd v')) vehs)
    -- putStrLn "Fastest track car / $"
    -- putStrLn $ show (findLastOrd (\v v' -> compare (spd $ tstats v) (spd $ tstats v')) tracked)
    -- putStrLn "Cheapest track car w/ consumables"
    -- TODO
