module Main where

import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Ord (Down(..))

import Lib
import Renderable
import Util

frs = VStats "frs" 2750 200 10000
wrx = VStats "wrx" 3250 300 24000
miata = VStats "miata" 2400 160 5000
s2k = VStats "s2000" 2800 240 18000
cayman = VStats "cayman" 3000 320 35000
c6 = VStats "c6" 3200 400 25000
c5z = VStats "c5z06" 3200 400 20000
e46 = VStats "e46" 3500 330 25000
e92 = VStats "e92" 3800 414 35000

orderF = (*(-1)) . spd
orderV v1 v2 = compare (orderF v1) (orderF v2)
-- orderTV (TV v1 _ cpy) (TV v2 _ cpy') = compare (orderF v1) (orderF v2)
orderTV (TV v1 _ cpy) (TV v2 _ cpy') = compare cpy cpy'

vehs = sortBy orderV [ frs, wrx, miata, s2k, cayman, c6, c5z, e46, e92 ]
tracked = sortBy orderTV [ trackV (modV c5z "track" 0 400 5000) 400 8 400,
    trackV (modV frs "NA" (-100) 190 10000) 250 4 250,
    trackV (modV frs "race" (-300) 200 15000) 250 4 250,
    trackV (modV frs "JRSC" 0 240 10000) 350 4 250,
    trackV (modV frs "JRSC race" 0 260 20000) 350 4 400,
    trackV (modV frs "JRSC +e85" 0 300 20000) 500 8 250,
    trackV (modV wrx "cobb" 0 300 5000) 350 6 350,
    trackV (modV s2k "NA" (-100) 200 5000) 250 4 300,
    trackV (modV miata "race" (-200) 160 10000) 200 4 200 ]

instance Row VStats where
    header v                 = ["weight", "p2w", "cost", "spd/$"]
    parts v@(VStats l w p c) = w +> roundn 2 (p2w v) <+> c <+> floor (spd v)
    label v@(VStats l _ _ _) = l

instance Row TV where
    header tv                 = ["weight", "p2w", "cost", "spd/$", "$/y"]
    parts tv@(TV v initc cpy) = parts v{ worth = worth v + initc } <+> cpy
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
