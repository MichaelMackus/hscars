module Main where

import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Ord (Down(..))

import Lib
import Util

frs = V "frs" 2750 200 10000
miata = V "miata" 2400 160 5000
s2k = V "s2000" 2800 200 18000
cayman = V "cayman" 3000 320 35000
c6 = V "c6" 3200 400 25000
c5z = V "c5z06" 3200 400 20000
e46 = V "e46" 3500 330 25000
e92 = V "e92" 3800 414 35000

orderF = (*(-1)) . spd
orderV v1 v2 = compare (orderF v1) (orderF v2)
orderMV v1 v2 = compare (orderF $ toV v1) (orderF $ toV v2)
orderTV v1 v2 = compare (orderF $ toV' v1) (orderF $ toV' v2)

vehs = sortBy orderV [ frs, miata, s2k, cayman, c6, c5z, e46, e92 ]
modded = sortBy orderMV [ ModV frs "JRSC" 240 7000,
    ModV frs "JRSC +e85" 300 10000,
    ModV s2k "JRSC" 360 5000,
    ModV miata "turbo" 200 5000 ]
tracked = sortBy orderTV [ TV (ModV frs "JRSC +track" 300 20000) 0,
    TV (ModV frs "N/A +track" 200 10000) 0,
    TV (ModV s2k "+track" 200 5000) 0,
    TV (ModV miata "+track" 200 10000) 0,
    TV (ModV c6 "track" 350 10000) 0,
    TV (ModV c5z "track" 350 10000) 0,
    TV (ModV (V "e30" 2000 180 5000) "+race" 180 5000) 0 ]
    --TV (ModV cayman "boltons" 280 4000 ]

main = do
        let header = ["weight", "p2w", "cost", "$/hp", "spd/$"]
            tables = [ ("Stock", vtable header label format vehs),
                       ("Modded", vtable header label format $ map toV modded),
                       ("Tracked", vtable header label format $ map toV' tracked) ]

        forM_ tables $ \t -> do
            putStrLn (fst t)
            putStrLn (render $ snd t)

        -- TODO calculate some useful statistics
        -- putStrLn "Fastest street car / $"
        -- TODO
        -- putStrLn "Fastest track car / $"
        -- TODO
        -- putStrLn "Cheapest track car w/ consumables"
        -- TODO
    where
        format v@(V l w p c) = [show w, show (round' 2 $ p2w v), show c, show (round' 2 $ dhp v), show (floor $ spd v)]
        round' n f           = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

        -- for pagination (not currently used)
        printTable :: String -> IO ()
        printTable t = putStrLn t >> putStrLn "Hit enter for more..." >> getLine >> return ()
