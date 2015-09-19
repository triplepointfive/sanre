{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
module Main where

import qualified Data.Map as Map
import Control.Monad

import Data.GraphViz hiding (graphToDot, X11Color(..))
import System.Console.CmdArgs

import Sanre.Dot
import Sanre.FileTree
import Sanre.Types
import Sanre.Graph

config :: Config
config = Config
    { directory = def &= args &= typDir &= opt "."
    , external  = def &= help "Include links to external modules"
    , color     = Black
    } &=
    help "Draws graph for Haskell modules dependencies" &=
    summary "Sanre v0.1, (C) Ilya Smelkov" &=
    details [
      "Sanre creates an image file with modules dependencies",
      "",
      "To check all Haskell files in 'src' and generate a report type:",
      "  sanre src"]

main :: IO ()
main = cmdArgs config >>= sanreMain

sanreMain :: Config -> IO ()
sanreMain config@Config{..} = do
    dirTree <- buildDirTree directory
    void $ addExtension (buildGraph config dirTree) Png "image"
