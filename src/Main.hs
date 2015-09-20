{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Map as Map
import Control.Monad

import Data.GraphViz (GraphvizOutput(Png), runGraphviz, addExtension)
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
    , uniq      = def &= help "Consider multiple imports of same module as one"
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
sanreMain config = do
    dirTree <- buildGraph config <$> buildDirTree (directory config)
    void $ addExtension (runGraphviz (graphToDot dirTree)) Png "image"
