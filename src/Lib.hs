module Lib where

data VStats = VStats {
    vlabel :: String,
    weight :: Int,
    bhp :: Int,
    worth :: Int
} deriving (Show)

data ModV = ModV { mstats :: VStats, cost :: Int } deriving (Show)
data TV = TV { tstats :: VStats, initcost :: Int, costperyear :: Int } deriving (Show)

modV :: VStats -> String -> Int -> Int -> Int -> ModV
modV v@(VStats l w p c) l' wdiff whp mcost = ModV { mstats = v{ bhp = whp, vlabel = l ++ " " ++ l', weight = w + wdiff }, cost = mcost }

trackV :: ModV -> Int -> Int -> Int -> TV
trackV mv@(ModV vst c) costpersess tiresperyear tirecost =
    let sessperyear = 12
        calcperyear = (costpersess * sessperyear) + (tiresperyear * tirecost)
    in TV { tstats = vst, initcost = c, costperyear = calcperyear }

p2w (VStats l w p _) = (fromIntegral w / fromIntegral p)
dhp (VStats _ _ p c) = (fromIntegral c / fromIntegral p)
-- speed is calculated as a division of p2w (greater speed = faster/lower p2w)
speed v = (1 / p2w v) * 1000
-- speed per dollar
spd v@(VStats _ _ _ c) = (speed v / fromIntegral c) * 10000
-- ownership offset of current cars worth
-- mdhp v@(V l _ p c) = dhp v{worth = c - (worth frs) }
mdhp v@(VStats l _ p c) = dhp v{worth = c - 10000 }
mspd v@(VStats l _ p c) = spd v{worth = c - 10000 }
