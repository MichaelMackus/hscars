module Main where

import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Ord (Down(..))

import Lib
import Renderable
import Util

frs = VStats "frs" 2750 200 10000
miata = VStats "miata" 2400 160 5000
s2k = VStats "s2000" 2800 200 18000
cayman = VStats "cayman" 3000 320 35000
c6 = VStats "c6" 3200 400 25000
c5z = VStats "c5z06" 3200 400 20000
e46 = VStats "e46" 3500 330 25000
e92 = VStats "e92" 3800 414 35000

orderF = (*(-1)) . spd
orderV v1 v2 = compare (orderF v1) (orderF v2)
-- orderTV (TV v1 _ cpy) (TV v2 _ cpy') = compare (orderF v1) (orderF v2)
orderTV (TV v1 _ cpy) (TV v2 _ cpy') = compare cpy cpy'

vehs = sortBy orderV [ frs, miata, s2k, cayman, c6, c5z, e46, e92 ]
tracked = sortBy orderTV [ trackV (modV c5z "track" 350 400 5000) 400 8 400,
    trackV (modV frs "NA" 160 190 10000) 250 4 250,
    trackV (modV frs "JRSC" 160 240 17000) 350 8 250,
    trackV (modV frs "JRSC +e85" 160 300 20000) 500 8 250,
    trackV (modV s2k "NA" 180 200 5000) 250 4 300,
    trackV (modV miata "NA" 130 160 5000) 200 4 200 ]

instance Row VStats where
    header v                 = ["weight", "p2w", "cost", "$/hp", "spd/$"]
    parts v@(VStats l w p c) = [show w, show (roundn 2 $ p2w v), show c, show (roundn 2 $ dhp v), show (floor $ spd v)]
    label v@(VStats l _ _ _) = l

instance Row TV where
    header tv                 = ["weight", "p2w", "cost", "$/hp", "spd/$", "$/y"]
    parts tv@(TV v initc cpy) = parts v{ worth = worth v + initc } ++ [show cpy]
    label tv@(TV v _ _)       = label v

main = do
    putStrLn "Stock"
    render vehs

    putStrLn "Tracked"
    render tracked

    -- calculate some useful statistics
    -- putStrLn "Fastest street car / $"
    -- putStrLn $ show (findLastOrd (\v v' -> compare (spd v) (spd v')) vehs)
    -- putStrLn "Fastest track car / $"
    -- putStrLn $ show (findLastOrd (\v v' -> compare (spd $ tstats v) (spd $ tstats v')) tracked)
    -- putStrLn "Cheapest track car w/ consumables"
    -- TODO
