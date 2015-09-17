{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import System.Environment

import Data.GraphViz.Exception
import Data.GraphViz.Commands

import Sanre.Dot
import Sanre.FileTree
import Sanre.Types

graphToDotPng :: FilePath -> Tree -> IO ()
graphToDotPng fpre g = void $ addExtension (runGraphviz (graphToDot g)) Png fpre

main :: IO ()
main = getArgs >>= \ (dir:_) -> buildDirTree dir >>= graphToDotPng "image"
