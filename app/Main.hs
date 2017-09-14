{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Csv
import Data.List (sortBy)
import Data.Maybe (fromJust, listToMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Args (getArgs, sort)
import Lib
import Renderable

instance FromNamedRecord VStats where
    parseNamedRecord r = VStats <$> r .: "car"
                                <*> r .: "weight"
                                <*> r .: "bhp"
                                <*> r .: "msrp"
                                <*> r .: "worth"
                                <*> r .: "mods"
                                <*> r .: "mpg"
                                <*> r .: "tires"

loadVehicles :: FilePath -> IO [VStats]
loadVehicles path = do
    csvData <- BL.readFile path
    case decodeByName csvData of
        Left err -> putStrLn err >> return []
        Right (_, v) -> return (V.toList v)

instance Row VStats where
    header v = ["weight", "p2w", "msrp", "cost", "$/y", "spd/$"]
    parts  v = weight v +> roundn 2 (p2w v) <+> msrp v <+> cost v <+> cpy v <+> floor (spd v)
        where roundn n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
    label  v = vlabel v

main = do
        args <- getArgs

        let load p = return . sortBy (sort args) =<< loadVehicles p

        stock  <- load "cars/stock.csv"
        modded <- load "cars/modded.csv"

        putStrLn "Stock"
        render (stock, stats stock)
        putStrLn ""
        putStrLn "Modified"
        render (modded, stats modded)
    where
        stats :: [VStats] -> [TableRow]
        stats vs = [TR ["car"] "Fastest / $"          [showv (findLComparing spd vs)],
                    TR ["car"] "Power / Weight"       [showv (findLComparing speed vs)],
                    TR ["car"] "Cheapest Cost"        [showv (findFComparing cost vs)],
                    TR ["car"] "Cheapest Consumables" [showv (findFComparing cpy vs)]]

            where
                showv = show . fromJust
                findComparing  f = sortBy (\v v' -> compare (f v) (f v'))
                findFComparing f = listToMaybe . findComparing f
                findLComparing f = listToMaybe . reverse . findComparing f

