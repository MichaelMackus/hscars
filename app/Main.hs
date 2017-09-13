{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Csv
import Data.List (sortBy)
import Data.Maybe (fromJust)
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
        let load p = return . sortBy orderV =<< loadVehicles p

        stock  <- load "cars/stock.csv"
        modded <- load "cars/modded.csv"

        putStrLn "Stock"
        render (stock, stats stock)
        putStrLn ""
        putStrLn "Modified"
        render (modded, stats modded)
    where
        stats :: [VStats] -> [TableRow]
        stats vs = [TR ["car"] "Fastest / $" [show (fromJust (findLastOrd (\v v' -> compare (spd v) (spd v')) vs))]]

        -- TODO "Cheapest track car w/ consumables"
