module Lib where

data VStats = VStats {
    vlabel :: String,
    weight :: Int,
    bhp :: Int,
    msrp :: Int,
    worth :: Int,
    modCost :: Maybe Int
} deriving (Show)

cost :: VStats -> Int
cost v =
    let mods = maybe 0 id (modCost v)
    in  worth v + mods

-- power to weight
p2w v = (fromIntegral (weight v) / fromIntegral (bhp v))
-- speed is calculated as a division of p2w (greater speed = faster/lower p2w)
speed v = (1 / p2w v) * 1000

-- speed per dollar
spd v = (speed v / fromIntegral (cost v)) * 10000
-- dollar per hp
dhp v = (fromIntegral (cost v) / fromIntegral (bhp v))
