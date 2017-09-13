module Renderable where

import Lib
import Util

import Data.List (intercalate)
import Text.Tabular
import qualified Text.Tabular.AsciiArt as A

instance Row VStats where
    header v                 = ["weight", "p2w", "msrp", "cost", "spd/$"]
    parts v@(VStats l lbs _ p _ _) = lbs +> roundn 2 (p2w v) <+> p <+> cost v <+> floor (spd v)
    label v@(VStats l _   _ _ _ _) = l

-- instance Row TV where
--     header tv                 = ["weight", "p2w", "cost", "spd/$", "$/y"]
--     parts tv@(TV v initc cpy) = parts v{ worth = worth v + initc } <+> cpy
--     label tv@(TV v _ _)       = label v

class Renderable r where
    render :: r -> IO ()

instance Row r => Renderable [r] where
    render rs = putStrLn $ A.render id id id mkTable
        where mkTable  = Table (Group NoLine mkLabels) (Group NoLine mkHeader) $ map parts rs
              mkLabels = map (Header . label) rs
              mkHeader = map Header . header $ head rs

class Row r where
    header :: r -> [String]
    parts :: r -> [String]
    label :: r -> String

-- helper to combine showables
(<+>) :: Show a => [String] -> a -> [String]
l <+> s = l ++ [show s]
infixl 5 <+>

(+>) :: (Show a, Show b) => a -> b -> [String]
(+>) a b = [show a, show b]
infixl 5 +>

-- basic renderable data structure
data TableRow = TR {
    th :: [String],
    tlabel :: String,
    td :: [String]
}

instance Row TableRow where
    header = th
    parts  = td
    label  = tlabel
