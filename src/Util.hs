module Util where

import Data.List (sortBy, reverse)
import Data.Maybe (listToMaybe)
import Data.Ord (Ordering(..))

roundn n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

-- explode :: forall a. forall b. Show b => [(a -> b)] -> a -> [String]
-- explode fs v = map showf fs
--     where showf f = show (f v)
