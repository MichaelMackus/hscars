module Args (Args(..), parseArgs, getArgs) where

import Lib

import Data.Ord (Down(..), comparing)
import qualified System.Environment as E

data Args = Args {
    sort :: (VStats -> VStats -> Ordering)
}

getArgs = E.getArgs >>= return . parseArgs

parseArgs :: [String] -> Args
parseArgs = Args . getSort
    where
        defaultSort = comparing (Down . spd)

        getSort []             = defaultSort
        getSort ("--sort":s:_) = sortf s
        getSort (_:xs)         = getSort xs

        sortf "weight"  = comparing weight
        sortf "cost"    = comparing cost
        sortf "cpy"     = comparing cpy
        sortf "p2w"     = comparing p2w
        sortf "speed"   = comparing (Down . speed)
        sortf "spd"     = comparing (Down . spd)
        sortf "dhp"     = comparing dhp
        sortf otherwise = defaultSort
