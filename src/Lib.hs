module Lib where

data VStats = VStats {
    vlabel :: String,
    weight :: Int,
    bhp :: Int,
    msrp :: Int,
    worth :: Int,
    modCost :: Int,
    mpg :: Int,
    tireCost :: Int
}

instance Show VStats where
    show = vlabel

cost :: VStats -> Int
cost v = worth v + modCost v

-- cost per year
costPerY :: VStats -> Int -> Int -> Int -> Int
costPerY v miles tireChanges gasCost = floor ((fromIntegral miles / fromIntegral (mpg v)) * (fromIntegral gasCost)) + (tireCost v * tireChanges)

-- cheater costPerY
cpy v = costPerY v miles tireChanges gasCost
    where
        miles       = 15000
        tireChanges = if bhp v >= 400 then 6 else 4
        gasCost     = 4

-- power to weight
p2w v = (fromIntegral (weight v) / fromIntegral (bhp v))
-- speed is calculated as a division of p2w (greater speed = faster/lower p2w)
speed v = (1 / p2w v) * 1000

-- speed per dollar
spd v = (speed v / fromIntegral (cost v)) * 10000
-- dollar per hp
dhp v = (fromIntegral (cost v) / fromIntegral (bhp v))
