{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Smelkov Ilya
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Smelkov Ilya <triplepointfive@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Dotted graphs renderer.
--
-----------------------------------------------------------------------------
module Sanre.Draw (
   graphToDot
 , graphToDotParams
 , vacuumParams
) where

import qualified Data.Map.Strict as Map

import Data.GraphViz hiding (graphToDot)
import Data.GraphViz.Attributes.Complete (
         Attribute(RankDir, Splines, FontName)
       , RankDir(..), EdgeType(SplineEdges)
       )

import Control.Arrow(second)

import Sanre.Types hiding (Color(..), color)
import qualified Sanre.Types (Color(..))

graphToDot :: Tree -> DotGraph String
graphToDot = graphToDotParams vacuumParams . Map.toList

graphToDotParams :: (Ord a, Ord cl) => GraphvizParams a () () cl l
                 -> [(a, [a])] -> DotGraph a
graphToDotParams params nes = graphElemsToDot params ns es
  where
    ns = map (second $ const ()) nes

    es = concatMap mkEs nes
    mkEs (f,ts) = map (\t -> (f,t,())) ts

vacuumParams :: GraphvizParams a () () () ()
vacuumParams = defaultParams { globalAttributes = gStyle  }

gStyle :: [GlobalAttributes]
gStyle = [ GraphAttrs attrs
         , NodeAttrs  [textLabel "\\N", shape Hexagon, fontColor Red]
         , EdgeAttrs  [color Green, style solid]
         ]
  where
    attrs = [RankDir FromBottom, Splines SplineEdges, FontName "courier"]
