module Lib where

import Control.Monad (forM_)
import Data.Bifunctor (bimap)

data VStats = VStats {
    label :: String,
    weight :: Int,
    bhp :: Int,
    worth :: Int
} deriving (Show)

data ModV = ModV { mstats :: VStats, cost :: Int } deriving (Show)
data TV = TV { tstats :: VStats, initcost :: Int, costperyear :: Int } deriving (Show)

modV :: VStats -> String -> Int -> Int -> Int -> ModV
modV v@(VStats l w p c) l' fromWhp toWhp mcost = ModV { mstats = v{ bhp = mbhp, label = l ++ " " ++ l' }, cost = mcost }
    where mbhp    = floor (fromIntegral p / mchange)
          mchange = (fromIntegral fromWhp / fromIntegral toWhp)

trackV :: ModV -> Int -> Int -> Int -> TV
trackV mv@(ModV vst c) costpersess tiresperyear tirecost =
    let sessperyear = 12
        calcperyear = (costpersess * sessperyear) + (tiresperyear * tirecost)
    in TV { tstats = vst, initcost = c, costperyear = calcperyear }

tlabel (TV v _ _) = label v

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
