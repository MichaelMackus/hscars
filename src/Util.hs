module Util where

import Data.List (sortBy, reverse)
import Data.Maybe (listToMaybe)
import Data.Ord (Ordering(..))

-- find function which finds first element based on order
findOrd :: (a -> a -> Ordering) -> [a] -> Maybe a
findOrd f = listToMaybe . sortBy f

findLastOrd :: (a -> a -> Ordering) -> [a] -> Maybe a
findLastOrd f = listToMaybe . reverse . sortBy f

roundn n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
