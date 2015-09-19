-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Smelkov Ilya
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Smelkov Ilya <triplepointfive@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Transforms dir tree according to input params.
--
-----------------------------------------------------------------------------
module Sanre.Graph (
  buildGraph
) where

import qualified Data.Map.Strict as Map

import Sanre.Types

-- | Accepts the dir tree and applies transformations listed in a config.
buildGraph :: Config -> Tree -> Tree
buildGraph _ tree = tree

-- | Removes nodes which does not presence in a tree. Usually it's
-- base or external libraries modules.
excludeExternal :: Tree -> Tree
excludeExternal tree = Map.map (filter (`Map.member` tree)) tree
