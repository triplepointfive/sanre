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
module Sanre.Dot (
   graphToDot
 , graphToDotParams
 , vacuumParams

) where

import qualified Data.Map.Strict as Map

import Data.GraphViz hiding (graphToDot)
import Data.GraphViz.Attributes.Complete (
         Attribute(RankDir, Splines, FontName)
       , RankDir(FromLeft), EdgeType(SplineEdges)
       )

import Control.Arrow(second)

import Sanre.Types

graphToDot :: Tree -> DotGraph String
graphToDot = graphToDotParams vacuumParams . Map.toList

graphToDotParams :: (Ord a, Ord cl) => GraphvizParams a () () cl l -> [(a, [a])] -> DotGraph a
graphToDotParams params nes = graphElemsToDot params ns es
  where
    ns = map (second $ const ()) nes

    es = concatMap mkEs nes
    mkEs (f,ts) = map (\t -> (f,t,())) ts

vacuumParams :: GraphvizParams a () () () ()
vacuumParams = defaultParams { globalAttributes = gStyle  }

gStyle :: [GlobalAttributes]
gStyle = [ GraphAttrs [RankDir FromLeft, Splines SplineEdges, FontName "courier"]
         , NodeAttrs  [textLabel "\\N", shape PlainText, fontColor Blue]
         , EdgeAttrs  [color Black, style dotted]
         ]
