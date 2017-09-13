module Renderable where

import Lib
import Util

import Text.Tabular
import qualified Text.Tabular.AsciiArt as A

class Renderable r where
    table  :: r -> String
    render :: r -> IO ()

class Row r where
    header :: r -> [String]
    parts :: r -> [String]
    label :: r -> String

instance Row VStats where
    header v                 = ["weight", "p2w", "msrp", "cost", "spd/$"]
    parts v@(VStats l lbs _ p _ _) = lbs +> roundn 2 (p2w v) <+> p <+> cost v <+> floor (spd v)
    label v@(VStats l _   _ _ _ _) = l

instance Row r => Renderable [r] where
    table rs = A.render id id id . Table (Group NoLine mkLabels) (Group NoLine mkHeader) $ map parts rs
        where mkLabels = map (Header . label) rs
              mkHeader = map Header . header $ head rs
    render = putStrLn . table

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
