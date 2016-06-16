module Renderable where

import Lib
import Util

import Data.List (intercalate)
import Text.Tabular
import qualified Text.Tabular.AsciiArt as A

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
