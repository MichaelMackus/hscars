module Lib where

import Control.Monad (forM_)
import Data.Bifunctor (bimap)

data V = V { label :: String, weight :: Int, bhp :: Int, worth :: Int }
data ModV = ModV { stock :: V, mod :: String, whp :: Int, cost :: Int }
data TV = TV { mods :: ModV, costpery :: Int }

toV (ModV (V l w _ m) mod whp c) = V (l ++ " " ++ mod) w (floor $ fromIntegral whp / 0.85) (m + c)
toV' (TV mv cpy) = toV mv

p2w (V l w p _) = (fromIntegral w / fromIntegral p)
dhp (V _ _ p c) = (fromIntegral c / fromIntegral p)
-- speed is calculated as a division of p2w (greater speed = faster/lower p2w)
speed v = (1 / p2w v) * 1000
-- speed per dollar
spd v@(V _ _ _ c) = (speed v / fromIntegral c) * 10000
-- ownership offset of current cars worth
-- mdhp v@(V l _ p c) = dhp v{worth = c - (worth frs) }
mdhp v@(V l _ p c) = dhp v{worth = c - 10000 }
mspd v@(V l _ p c) = spd v{worth = c - 10000 }
