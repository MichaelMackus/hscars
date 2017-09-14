module Renderable where

import Data.List (sort)
import Text.Tabular
import qualified Text.PrettyPrint as Pretty
import qualified Text.Tabular.AsciiArt as A

class Renderable r where
    table  :: r -> String
    render :: r -> IO ()

class Row r where
    header :: r -> [String]
    parts :: r -> [String]
    label :: r -> String

instance Row r => Renderable [r] where
    table  = mkTable
    render = putStrLn . table

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

-- allow rendering two instances side by side
instance (Renderable r, Renderable r') => Renderable (r, r') where
    table  (r, r') =
        let (t,    t')       = (table r, table r')
            docs             = map mkPretty (zip (lines t) (fill (lines t')))
            mkPretty (l, l') = Pretty.text l Pretty.$$ Pretty.nest (maxWidth t + 5) (Pretty.text l')
            maxWidth         = last . sort . map length . lines
            fill          xs = xs ++ replicate ((length (lines t)) - length xs) ""
        in  show (Pretty.vcat docs)
    render = putStrLn . table

mkTable rs = A.render id id id . Table (Group NoLine mkLabels) (Group NoLine mkHeader) $ map parts rs
        where mkLabels = map (Header . label) rs
              mkHeader = map Header . header $ head rs

-- helper to combine showables
(<+>) :: Show a => [String] -> a -> [String]
l <+> s = l ++ [show s]
infixl 5 <+>

(+>) :: (Show a, Show b) => a -> b -> [String]
(+>) a b = [show a, show b]
infixl 5 +>
