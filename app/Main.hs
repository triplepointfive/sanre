module Main where

import Lib
import Language.Haskell.GhcMod

main :: IO ()
main = runGhcModT defaultOptions (loadSymbolDb "../soten/soten.cabal") >>= print
