{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE RecordWildCards     #-}
module Main where

import Control.Monad

import Data.GraphViz.Exception
import Data.GraphViz.Commands
import System.Console.CmdArgs

import Sanre.Dot
import Sanre.FileTree
import Sanre.Types

graphToDotPng :: FilePath -> Tree -> IO ()
graphToDotPng fpre g = void $ addExtension (runGraphviz (graphToDot g)) Png fpre

data Config = Config
    { directory :: String
    } deriving (Show, Data, Typeable)

config :: Config
config = Config
    { directory = "." &= help "Path to a directory with sources"
    } &= summary "Sanre v0.1"

main :: IO ()
main = cmdArgs config >>= sanreMain

sanreMain :: Config -> IO ()
sanreMain Config{..} = buildDirTree directory >>= graphToDotPng "image"
