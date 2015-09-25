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

import qualified Sanre.Types as San

graphToDot :: San.Config -> San.Tree -> DotGraph String
graphToDot config = graphToDotParams params . Map.toList
  where
    params = vacuumParams config

graphToDotParams :: (Ord a, Ord cl) => GraphvizParams a () () cl l
                 -> [(a, [a])] -> DotGraph a
graphToDotParams params nes = graphElemsToDot params ns es
  where
    ns = map (second $ const ()) nes

    es = concatMap mkEs nes
    mkEs (f,ts) = map (\t -> (f,t,())) ts

vacuumParams :: San.Config -> GraphvizParams a () () () ()
vacuumParams config = defaultParams { globalAttributes = gStyle config }

gStyle :: San.Config -> [GlobalAttributes]
gStyle config =
    [ GraphAttrs graphAttrs
    , NodeAttrs  nodeAttrs
    , EdgeAttrs  edgeAttrs
    ]
  where
    edgeAttrs  =
        [ color (mapColor (San.color config))
        , style solid
        ]
    graphAttrs =
        [ RankDir FromBottom
        , Splines SplineEdges
        , FontName "courier"
        ]
    nodeAttrs =
        [ textLabel "\\N"
        , shape Hexagon
        , fontColor Red
        ]

mapColor :: San.Color -> X11Color
mapColor San.Blue  = Blue
mapColor San.Black = Black
mapColor San.Green = Green
