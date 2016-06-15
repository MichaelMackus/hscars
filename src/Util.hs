module Util where

import Control.Applicative
import Data.List (intercalate)
import Data.Ord (Ordering(..))

import Text.Tabular
import qualified Text.Tabular.AsciiArt as A

-- create table based on header & list of vehicles
-- TODO look into tabular library
table :: String -> [String] -> [[String]] -> String
table header rheader rows = unlines $
        [ header,
          line,
          tabbed rheader,
          unlines $ map tabbed rows
        ]
    where
        line = replicate (length header) '-'

-- take a list of Vs and turn it into a Table
vtable :: [String] -> (a -> String) -> (a -> [String]) -> [a] -> Table String String String
vtable header labelf f vs = Table (Group NoLine $ map (Header . labelf) vs) (Group NoLine $ map Header header) rows
    where rows = map f vs

-- create table based on header & list of vehicles
tabular :: [String] -> [String] -> [[String]] -> Table String String String
tabular ch rh rows = Table (Group NoLine $ map Header ch) (Group NoLine $ map Header rh) rows

-- simple render func
render :: Table String String String -> String
render = A.render id id id

-- spaces str with tabs
tabbed :: [String] -> String
tabbed s = intercalate "\t" s
    where tabs = concat $ replicate (floor $ (fromIntegral diff) / 4) "\t"
          diff = maxlength s - minlength s

-- find min length of list of strs
minlength :: [String] -> Int
minlength = foldr (comparelen LT) 0

-- find max length of list of strs
maxlength :: [String] -> Int
maxlength = foldr (comparelen GT) 0

comparelen :: Ordering -> String -> Int -> Int
comparelen ord s i = if compare (length s) i == ord then length s else i
