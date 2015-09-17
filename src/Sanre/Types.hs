-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Smelkov Ilya
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Smelkov Ilya <triplepointfive@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Common types representing module dependencies.
--
-----------------------------------------------------------------------------
module Sanre.Types where

import qualified Data.Map.Strict as Map

-- | A module and it's imports.
type Node = (String, [String])

-- | Represents the whole graph, contains the whole module names.
type Tree = Map.Map String [String]
